(put 'config 'rcsid 
 "$Id$")

(eval-when-compile (setq byte-compile-warnings '(not cl-functions)))

(require 'cl)

(require 'advice)

(defvar *debug-config-error* t)

(defvar *configdir* "~/emacs/config/")

(defvar *config-file-name-member*
  (if (eq window-system 'w32)
      'member-ignore-case
    'member)
  "function to apply to determine filename equivalence")

(defvar *share* (expand-file-name (or (getenv "SHARE") "/usr/share/emacs")))
(defvar *emacsdir* (expand-file-name
		    (or (getenv "EMACS_DIR")
			(getenv "EMACSDIR")
			(and (getenv "EMACSPATH")
			     (concat (getenv "EMACSPATH") "/.."))
			*share*
			)))


;; this advice allows pre- and post- hooks on all loaded features
;; this way customization can be tailored to the feature instead of all lumped together
;; also see eval-after-load

(defvar *debug-pre-load-hook* nil)
(defvar *debug-post-load-hook* nil)

; (setq *debug-pre-load-hook* t *debug-post-load-hook* t)
; (setq *debug-pre-load-hook* nil *debug-post-load-hook* nil)

(defvar *debug-pre-load-with-code-conversion-hook* nil)
(defvar *debug-post-load-with-code-conversion-hook* nil)
; (setq  *debug-pre-load-with-code-conversion-hook* t  *debug-post-load-with-code-conversion-hook* t)
; (setq  *debug-pre-load-with-code-conversion-hook* nil  *debug-post-load-with-code-conversion-hook* nil)

(defvar *disable-load-hook* nil)

; (defvar *debug-config-list* '(xdb))
(defvar *debug-config-list* nil
  "list of atoms or strings representing names of functions to trap on pre- or post- load
specify string to trap an explicit load, specify an atom to trap a require") 

(defvar *config-log-hook* nil "if set, log configured pre- and post- load actions to message buffer")

; a couple of helpr functions
(defun add-to-load-path-p (dir &optional append)
  "maybe add DIR to `load-path', if it is a directory and not already there.
add to front of list unless optional APPEND is set.  see `add-to-load-path'
"
  (let ((dir (expand-file-name dir)))
    (when (file-directory-p dir)
      (add-to-load-path dir append)
      (load-autoloads dir)
      )
    )
  )

(defun find-dir-in-path (dir path &optional predicate)
  "returns DIR expanded along members of directory list PATH, if it exists and is a directory, else nil.
dir must also satisfy optional PREDICATE of one arg, if specified
"
  (loop
   with d
   for p in path
   when (and (file-directory-p (setq d (expand-file-name dir p))) (or (not predicate) (funcall predicate d)))
   return d)
  )

(defun find-file-in-path (file &optional path predicate)
  "finds FILE expanded along members of directory list PATH, if it exists visit it, else nil.
default path is  `load-path'
file must also satisfy optional PREDICATE of one arg, if specified
"
  (let ((f (loop
	    with v
	    for p in (or path load-path)
	    when (and (file-exists-p (setq v (expand-file-name file p))) (or (not predicate) (funcall predicate v)))
	    return v)))
    (when (and f (file-exists-p f) (find-file f)))
    )
  )
; (find-file-in-path "pre-w3m.el")

(defun loadp (prefix file)
  (let* ((f0 (file-name-sans-extension (file-name-nondirectory (format "%s" file))))
	 (f1 (format "%s%s" prefix f0))
	 (f2 (loop for x in load-path when (file-exists-p (format "%s/%s.el" x f1)) collect (format "%s/%s.el" x f1)))
	 (do-message (not *config-log-hook*))
	 )
    
    (loop for f in f2 do (load f t do-message))
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

  (let ((arg (ad-get-arg 0)))

    (unless *disable-load-hook*
      (and *debug-pre-load-hook* (debug))
      (loadp "pre-" arg)
      )

    ad-do-it

    ;; tbd -- this has a bug if the hooked module has been required inside (eval-when-compile (require ...))

    (unless *disable-load-hook*
      (and *debug-post-load-hook* (debug))
      (loadp "post-" arg)
      )

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
; (if (ad-is-advised 'load) (ad-unadvise 'load))
; (setq *debug-post-load-hook* t)


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
    (and *debug-pre-load-with-code-conversion-hook* (debug))
    (loadp "pre-" (ad-get-arg 0))
    )

  ad-do-it

  (unless *disable-load-hook*
    (and *debug-post-load-with-code-conversion-hook* (debug))
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

; (if (ad-is-advised 'load-with-code-conversion) (ad-unadvise 'load-with-code-conversion))
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
	    (mapc #'(lambda (y) 
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
	       (load parent t (not *config-log-hook*))
	     parent)
	   )
	 )
    )
  )

(defun config-ancestors (&optional module)
  "returns the ancestor list of optional MODULE
that is the list of modules of the same base name along load-path.  
default value module is `this-load-file'
"

  (let* ((module (or module (this-load-file)))
	 (y (file-name-sans-extension (file-name-nondirectory module)))
	 (dir (file-name-directory module))
	 (l (loop
	     for x in load-path 
	     with z = nil
	     when
	     (or
	      (file-exists-p (setq z (concat x "/" y ".elc")))
	      (file-exists-p (setq z (concat x "/" y ".el"))))
	     collect z)))
    (cdr (member dir l))
    )
  )
; (config-ancestors "~/emacs/config/hosts/granite/keys.el")
; (config-ancestors "~/emacs/config/os/w32/keys.el")
; (locate-file "keys" load-path '(".el" ".elc"))

(defun find-config-parent (&optional module)
  " visit the immediate parent of MODULE
if there is a choice between compiled and source versions of the parent, prefer the source
"
  (interactive)
  (let* (
	 (module (or module (this-load-file)))
	 (y (file-name-sans-extension (file-name-nondirectory module)))
	 (l (config-ancestors module))
	 (parent (and l
		      (let ((z (expand-file-name (concat y ".el") (file-name-directory (car l))))) (and (file-exists-p z) z))))
	 )
        parent
    )
  )
; (find-config-parent "~/emacs/config/hosts/granite/keys.el")
; (find-config-parent "~/emacs/config/hosts/granite/post-xz.el")
; (find-config-parent "~/emacs/config/os/w32/keys.el")
; (locate-dominating-file FILE NAME)

(defun visit-config-parent (&optional module)
  (let ((parent (find-file module) ))
    (and parent (file-exists-p parent) (find-file parent))
    )
  )


(eval-when-compile
					; suppress callargs byte-compile-warnings  we know what we're doing?
  (let ((byte-compile-warnings (and (boundp 'byte-compile-warnings) byte-compile-warnings)))

    (if (listp byte-compile-warnings) 
	(add-to-list 'byte-compile-warnings 'callargs)
      (setq byte-compile-warnings '(callargs))
      )


    (defmacro called-interactively-p* (&optional kind)
      "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
      (cond
       ((not (fboundp 'called-interactively-p))
	'(interactive-p))
       ((> 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
	`(called-interactively-p ,kind))
       (t
	`(called-interactively-p))
       ))

    )
  )


(defun locate-config-file (fn)
  "find CONFIG along load-path.
searches first for config unadorned, then with extension .el
returns full path name.
"
  (let ((afn (loop for a in load-path
		   thereis (let ((afn (format "%s/%s" a fn)))
			     (or (and (file-exists-p afn) afn)
				 (and (file-exists-p (setq afn (concat afn ".el"))) afn))
			     )
		   )))
    afn)
  )
; (locate-config-file "host-init")
; (locate-config-file "os-init")

(defun find-config-file (fn)
  "visit CONFIG along load-path, if it exists.
see `locate-config-file'"

  (interactive "sconfig file: ")
  (let ((afn (locate-config-file fn)))
    (if afn (find-file afn) 
      (message "%s not found along load-path" fn)
      )
    )
  )

;;; autoload os-init
; autoload host-init
(loop for x in '("os-init" "host-init" "common-init") ; more tbd?
      do
      (eval
       `(defun ,(intern x) ()
	  ,(format "shortcut for `find-config-file' \"%s\"
with optional prefix arg, dired containing directory
" x)
	  (interactive)

	  (let* ((config  ,x)
		 (fn (locate-config-file config)))
	    (if (null fn)
		(message (format "config %s not found along load-path." fn))
	      (if current-prefix-arg (dired (file-name-directory fn))
		(find-file fn) 
		)
	      )
	    )
	  )
       )
      )
; (os-init)
; (host-init)
; (common-init)

(defun emacs-version-init ()
  "shortcut for `find-config-file' \"EmacsXX\" where XX=`emacs-major-version'"
  (interactive)
  (let ((emacs-major-version-init-file-name (format  "Emacs%d" emacs-major-version)))
    (find-config-file emacs-major-version-init-file-name)
    )
  )

(defun hostrc ()
  (interactive)
  "find host specific shell rc"

  (let ((hostname (system-name)))
    (cond
     ((null hostname)
      (when (called-interactively-p* 'any) (message "hostname not defined")))
     (t
      (let ((dir (expand-file-name hostname "~/config/hosts")))
	(cond 
	 ((not (file-directory-p dir))
	  (when (called-interactively-p* 'any) (message "%s not found" dir)))
	 (t
	  (let ((rcfile (expand-file-name ".bashrc" dir)))
	    (cond
	     ((not (file-exists-p rcfile))
	      (when
		  (called-interactively-p* 'any) (message "%s not found" rcfile)))
	     (t
	      (find-file rcfile))
	     )
	    )
	  )
	 )
	)
      )
     )
    )
  )
; (call-interactively 'hostrc)

(defvar *last-host-config-thing* ".")
(defvar *host-config-dir* (file-name-as-directory (expand-file-name (system-name) "~/config/hosts")))

(defun host-config (&optional thing)
  "find shell config dir"

  (interactive
   (list
    (let* (
	   (completion-list
	    (mapcar (function (lambda (x) (list (file-name-nondirectory x) x))) (split (eval-process "find" *host-config-dir* "-type" "f") "\n")))
	   (thing (cadr (assoc (completing-read* "visit host config file (%s): " completion-list *last-host-config-thing* '(nil t)) completion-list))))
      thing)))

  (let* ((thing (expand-file-name thing *host-config-dir*)))

    (cond 
     ((null thing) (dired *host-config-dir*))
     ((file-directory-p thing) (dired thing))
     (t (find-file thing))
     )
    )
  )

; (call-interactively 'host-config)
; (host-config)

(defun host-bin ()
  (interactive)
  "find shell bin dir"
  (let* ((host-config (host-config))
	 (dir (and host-config (expand-file-name "bin" host-config))))
    (cond
     ((and (interactive-p) (file-directory-p dir))
      (dired dir))
     ((file-directory-p dir) 
      dir)
     ((interactive-p)
      (message "%s does not exist" dir)))
    )
  )
; (call-interactively 'host-bin)
; (host-bin)

(provide 'config-lib)

