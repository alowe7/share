(put 'config 'rcsid 
 "$Id: config.el 937 2011-01-09 20:40:38Z a $")

(require 'advice)
(require 'cl)

(setq *debug-config-error* t)

(defvar *configdir* "~/emacs/config/")

(defvar *config-file-name-member*
  (if (eq window-system 'w32)
      'member-ignore-case
    'member)
  "function to apply to determine filename equivalence")

(defvar share (expand-file-name (or (getenv "SHARE") "/usr/share/emacs")))
(defvar emacsdir (expand-file-name
		(or (getenv "EMACS_DIR")
		    (getenv "EMACSDIR")
		    (and (getenv "EMACSPATH")
			 (concat (getenv "EMACSPATH") "/.."))
			share
		    )))


;; this advice allows pre- and post- hooks on all loaded features
;; this way customization can be tailored to the feature instead of all lumped together
;; also see eval-after-load

(defvar *debug-pre-load-hook* nil)
(defvar *debug-post-load-hook* nil)

; (setq *debug-pre-load-hook* t *debug-post-load-hook* t)
; (setq *debug-pre-load-hook* nil *debug-post-load-hook* nil)

(defvar *disable-load-hook* nil)

; (defvar *debug-config-list* '(xdb))
(defvar *debug-config-list* nil
  "list of atoms or strings representing names of functions to trap on pre- or post- load
specify string to trap an explicit load, specify an atom to trap a require") 

(defvar *config-log-hook* nil "if set, log configured pre- and post- load actions to message buffer")

; a couple of helpr functions
(defun add-to-load-path-p (dir &optional append)
  (let ((dir (expand-file-name dir)))
    (when (file-directory-p dir)
      (add-to-load-path dir append)
      (load-autoloads dir)
	
      )
    )
  )

(defun loadp (prefix file)
  (let* ((f0 (file-name-sans-extension (file-name-nondirectory (format "%s" file))))
	 (f1 (format "%s%s" prefix f0))
	 (f2 (loop for x in load-path when (file-exists-p (format "%s/%s.el" x f1)) collect (format "%s/%s.el" x f1))))
    
    (cond ((null f2)
	   (and *config-log-hook*
		(message (format  "no matches for %s found in %s" f1 (pp load-path)))))
	  (t
	   (loop for f in f2 do
		 (load f t t)
		 (and *config-log-hook*
		      (message (format  "(loadp %s)" f))))))
    )
  )


(defadvice load (around 
		 hook-load
		 first 
		 activate)

  "hook (load f) to optionally (load pre-f) (load f) (load post-f)
no errors if files don't exist.
 "
  (if (ad-has-enabled-advice 'load 'around)
      (progn
	(ad-disable-advice 
	 'load
	 'around
	 'hook-load)

	(ad-activate 'load))
    )

  (unless *disable-load-hook*
    (and *debug-pre-load-hook* (debug))
    (loadp "pre-" (ad-get-arg 0))
    )

  ad-do-it

;; tbd -- this has a bug if the hooked module has been required inside (eval-when-compile (require ...))

  (unless *disable-load-hook*
    (and *debug-post-load-hook* (debug))
    (loadp "post-" (ad-get-arg 0))
    )

  (if (and (ad-has-any-advice 'load)
	   (not (ad-has-enabled-advice 'load 'around)))
      (progn
	(ad-enable-advice 
	 'load
	 'around
	 'hook-load)
	(ad-activate 'load)
	))
  )

(defadvice load-with-code-conversion (around 
		 hook-load-with-code-conversion
		 first 
		 activate)

  "hook (load-with-code-conversion f) to optionally (load pre-f) (load-with-code-conversion f) (load post-f)
no errors if files don't exist.
 "
  (if (ad-has-enabled-advice 'load-with-code-conversion 'around)
      (progn
	(ad-disable-advice 
	 'load-with-code-conversion
	 'around
	 'hook-load-with-code-conversion)

	(ad-activate 'load-with-code-conversion))
    )

  (unless *disable-load-hook*
    (and *debug-pre-load-hook* (debug))
    (loadp "pre-" (ad-get-arg 0))
    )

  ad-do-it

  (unless *disable-load-hook*
    (and *debug-post-load-hook* (debug))
    (loadp "post-" (ad-get-arg 0))
    )

  (if (and (ad-has-any-advice 'load-with-code-conversion)
	   (not (ad-has-enabled-advice 'load-with-code-conversion 'around)))
      (progn
	(ad-enable-advice 
	 'load-with-code-conversion
	 'around
	 'hook-load-with-code-conversion)
	(ad-activate 'load-with-code-conversion)
	))
  )

; (ad-has-enabled-advice 'load 'around)
; (ad-unadvise 'load)
; (ad-is-advised 'load)

; (defvar *debug-require-hook* t)

(defadvice require (around 
		    hook-require
		    first 
		    activate)

  "hook (require f) to optionally (load pre-f) (require f) (load post-f)
only if (not (featurep f))
no errors if files don't exist.
 "
  (if (featurep (ad-get-arg 0)) nil

    (if (ad-is-advised 'require)
	(ad-disable-advice 
	 'require
	 'around
	 'hook-require))

    (ad-activate 'require)

    (let ((arg (ad-get-arg 0)))
  ; 		(and *debug-require-hook* (debug))
      (loadp "pre-" arg)

      ad-do-it
  ; 		(and *debug-require-hook* (debug))

      (loadp "post-" arg)
      )

    (ad-enable-advice 
     'require
     'around
     'hook-require)
    (ad-activate 'require)
    )
  )

; (if (ad-is-advised 'require) (ad-unadvise 'require))

; warning: this fails for any built in functions, so ...
; we need to get fancy

(defmacro make-hook-name (fname)
  `(intern
    (concat "hook-" ,(symbol-name (eval fname)))
    )
  )
; (make-hook-name 'dired-mode)

(defvar add-to-load-path-hook nil
  "hook function called whenever a directory is added to load path via `add-to-load-path'. 
note that since add-to-load-path is recursive, this hook will get called at every level where `load-path' is modified." )

(defun load-autoloads (x)
  ; backward compatibility only .. autoloads should come from share/site-lisp/site-start.d
  (let ((f (concat x "/.autoloads")))
    (if (file-exists-p f)
	(condition-case x (load f nil t) (error  nil))
      )
    )
  )

(unless (fboundp 'read-directory-name)
  (fset 'read-directory-name 'read-file-name)
  )

(defun append-to-list (l m) 
  "adds to the value of LIST-VAR the element ELEMENT
like `add-to-list' except if element is added, it is added to the end of the list"
  (add-to-list l m t)
  )

(defvar *load-path-exclude-pattern* "\(/CVS\)\|\(\.svn\)$"
  "regexp for directories to prune from recursive `add-to-load-path' calls"
  )

(defun add-to-load-path (x &optional append subdirs)
  "add ELEMENT to `load-path` if it exists, and isn't already there.
by default add to the head of the list.  with optional arg APPEND add at the end of the list
with optional second arg SUBDIRS, add all subdirectories as well.

if successful, runs the value of `add-to-load-path-hook` and returns the new value of load-path.

returns nil otherwise.
"
  (interactive (list (read-directory-name "Add to load-path: ")))
  (if (and
       (file-directory-p x)
       (and (boundp '*config-file-name-member*) (functionp *config-file-name-member*)
	    (not (funcall *config-file-name-member* x load-path)))
       (and (boundp '*load-path-exclude-pattern*) (stringp *load-path-exclude-pattern*)
	    (not (string-match *load-path-exclude-pattern* x))))

      (progn

	(add-to-list 'load-path (expand-file-name x) append)

	(if subdirs
	    (mapc '(lambda (y) 
		       (if (and (file-directory-p (concat x "/" y)) (not (string= y ".")) (not (string= y "..")))
			   (add-to-load-path (concat x "/" y) append subdirs)))
		    (directory-files x)))

	(load-autoloads x)

	(run-hooks 'add-to-load-path-hook)
	
	load-path
	)
    )
  )

; we need to apply some here on first invocation, since some functions come preloaded.

(defun post-load (module) 
  "load any post-* modules for MODULE.
module may be a symbol or string.  
the post-module is constructed as \"post-module\", and loaded if found along `load-path'
"
  (interactive "smodule: ")
;  (debug)
  (load (format "post-%s" module) t t)
  )

(defun post-after-load (module)
  "if module is preloaded, also has any after-load forms, evaluate them.
"
  (eval-after-load module (post-load module))
  )

(defun load-list (pat)
  (interactive "spat: ")
  (mapconcat 'identity (loop for x in load-history when (string-match pat (car x)) collect (car x)) " ")
  )
; (load-list "post-cc")

; this helper function gives init files a weak inheritance capability

(defun this-load-file () (if load-in-progress load-file-name (buffer-file-name)))

(defun chain-parent-file (&optional arg)
  "return the parent of a load file.
this will be the one following it in load-path, if any.
with optional ARG, loads the file if found.

the current load file is determined by testing `load-in-progress'
if this is set, then the load file is given by `load-file-name',
otherwise it is given by `buffer-file-name'

this mechanism allows a sort of inheritance among load files.
a load file sitting in front of its 'parent' on the load-path can extend its settings by pre-chaining
or override them by post-chaining.
"
  (let ((f (this-load-file)))
    (and f
	 (let* (z
		(y (file-name-sans-extension (file-name-nondirectory f)))
		(l 
		 (loop for x in load-path when 
		       (or
			(file-exists-p (setq z (concat x "/" y ".elc")))
			(file-exists-p (setq z (concat x "/" y ".el"))))
		       collect z))
		(tail (funcall *config-file-name-member* f l))
		(parent (and tail (cadr tail))))

  ; before we load the parent, see if we have a rcsid
	   
	   (let* ((base (intern (file-name-sans-extension y)))
		  (rcsid (get base 'rcsid))
		  (rcsid-chain (get base 'rcsid-chain)))
	     (if rcsid
		 (progn (push rcsid rcsid-chain) 
			(put base 'rcsid-chain rcsid-chain)))
	     )

	   (if (and arg parent)
	       (load parent)
	     parent)
	   )
	 )
    )
  )

(defun find-parent-file ()
  "visit the parent of this file see `chain-parent-file'"

  (interactive)
  (let ((f (chain-parent-file)))
    (if (and f (file-exists-p f))
	(find-file f)
      (message "no parent found"))))

; directory-files appears to have a bug matching arbitrary regexps.

(defun get-directory-files (&optional directory full match)
  "return directory contents as a list of strings, excluding . and ..
see `directory-files'
"
  (interactive "sName: ")

  (loop for x in 
	(directory-files (or directory ".") full match)
	when 
	(let ((z (file-name-nondirectory x)))
	  (not (or (string= z ".") (string= z ".."))))
	collect x)
  )

(defvar hooked-preloaded-modules nil
  "list of preloaded modules.  if there's any load-hooks for these, they need to be run at init time
members may be symbols or strings, see `post-load'
"
  )

;; this constructs a load path from lisp and site-lisp dirs under HOME, EMACSDIR and SHARE
;; platform and host specific stuff come from config/hosts/HOSTNAME and config/os/UNAME

; these go at the head of the list
(condition-case err
    (mapc 'add-to-load-path
     (nconc 
      (and emacsdir (directory-files (concat emacsdir "/site-lisp") t "^[a-zA-Z]"))
      (and share
	   (nconc (directory-files (concat share "/site-lisp") t "^[a-zA-Z]")
		  (list (concat share "/site-lisp"))))
      )
     )
  (file-error t)
  )

; load any init files out there
(require 'find-func)
(and share
     (let ((site-start.d (concat share "/site-lisp/site-start.d")))
       (mapc 'load (and (file-directory-p site-start.d) (get-directory-files site-start.d)))
       )
     )

; these go at the head of the list
(mapc
 'add-to-load-path
 (list 
  (expand-file-name  "common" *configdir*)
  (expand-file-name (concat "os/" system-configuration)  *configdir*)
  (expand-file-name (concat "os/" (symbol-name window-system)) *configdir*)
  (expand-file-name (concat "hosts/"  (system-name)) *configdir*)
  (expand-file-name (concat (format "%d.%d" emacs-major-version emacs-minor-version)) *configdir*)

  (expand-file-name (concat (format "%d" emacs-major-version)) *configdir*)
  )
 )


; todo -- put this logic on a eval when loaded form?
(condition-case x
    (loop for x in hooked-preloaded-modules
	  do
	  (or 
	   (member
	    `(,x (post-load ,x)) after-load-alist)
	      (push 
	       `(,x (post-load ,x)) after-load-alist))
	  )
  ; (pop after-load-alist)

  ; make sure load-history is correct
  (unless (loop for x in load-history
		thereis (string-match (concat "fns-" emacs-version) (car x)))
    (load
     (format "%s/fns-%s" 
	     (getenv "EMACSPATH")
	     emacs-version)))

  (error (progn (message "some kind of random error in %s" (if load-in-progress load-file-name (buffer-file-name))) 
		(if *debug-config-error* (debug))))
  )

(or 
 (and (boundp 'window-system) (load (symbol-name window-system) t t))	; window system specific
 (and (getenv "TERM") (load (getenv "TERM") t t))		;terminal specific
)

(load (format "Emacs%d" emacs-major-version) t t)	;; optional emacs-version specific overrides

(load (format "Emacs%d.%d" emacs-major-version emacs-minor-version) t t)	;; optional emacs-minor-version specific overrides

(load "os-init" t t)		; optional os specific info

(load "window-system-init" t t)		; optional window-system specific info

(load "host-init" t t)		; optional host specific info

(provide 'config)

