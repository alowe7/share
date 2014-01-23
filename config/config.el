(put 'config 'rcsid 
 "$Id$")

(eval-when-compile (setq byte-compile-warnings '(not cl-functions)))

(require 'cl)
(require 'advice)
(require 'config-lib)

(defvar *debug-config-error* t)

(defvar *configdir* (expand-file-name "~/emacs/config/"))

(defvar hooked-preloaded-modules nil
  "list of preloaded modules.  if there's any load-hooks for these, they need to be run at init time
members may be symbols or strings, see `post-load'
"
  )

(unless (fboundp 'hostname) (fset 'hostname 'system-name))

;; this constructs a load path from lisp and site-lisp dirs under HOME, EMACSDIR and SHARE
;; platform and host specific stuff come from config/hosts/HOSTNAME and config/os/UNAME

; these go at the head of the list
(condition-case err
    (mapc 'add-to-load-path
     (nconc 
      (and *emacsdir* (directory-files (concat *emacsdir* "/site-lisp") t "^[a-zA-Z]"))
      (and *share*
	   (nconc (directory-files (concat *share* "/site-lisp") t "^[a-zA-Z]")
		  (list (concat *share* "/site-lisp"))))
      )
     )
  (file-error t)
  )

; load any init files out there
(require 'find-func)
(defvar *site-start-load-history* nil)
(setq  *site-start-load-history*
       (and *share*
	    (let ((site-start.d (concat *share* "/site-lisp/site-start.d")))
	      (mapc
	       #'(lambda (f) (load f t t))
	       (and (file-directory-p site-start.d)
		    (remove* '("." "..") (directory-files site-start.d) :test #'(lambda (x y) (member y x)))
		    )
	       )
	      )
	    )
       )

; these go at the head of the list
; tbd -- deprecated with generated autoloads
(mapc
 'add-to-load-path
 (list 
  (expand-file-name  "common" *configdir*)
  (expand-file-name (concat (format "%d" emacs-major-version)) *configdir*)
  (expand-file-name (concat (format "%d.%d" emacs-major-version emacs-minor-version)) *configdir*)
  (expand-file-name (concat "os/" system-configuration)  *configdir*)
  (expand-file-name (concat "os/" (symbol-name window-system)) *configdir*)
  (expand-file-name (concat "hosts/"  (system-name)) *configdir*)
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

  (error (progn (message "some kind of shouldn't happen error in %s" (if load-in-progress load-file-name (buffer-file-name))) 
		(if *debug-config-error* (debug))))
  )

;;
;; load-path is set up.  load configuration specific initializations
;;

(load "common-init" t t)		; optional common initialization

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

