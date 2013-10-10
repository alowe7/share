(put 'make-autoloads 'rcsid
 "$Id$")

 (eval-when-compile (require 'cl))

; this is a rewrite of make-autoloads from perl

; (defvar *autoload-definition-list* '(defun defun* defmacro define-derived-mode fset *defalias autoload))
(defvar *autoload-definition-list* '(defun defun* defmacro define-derived-mode *defalias))

(defun find-defs (filelist compiled)
  "produce a list of all definitions that might be autoloaded from files"

  (let* ((dir (car filelist))
	 (files (cadr filelist)))

    (loop for f in files
	  collect
	  (cons
	   (if compiled (replace-regexp-in-string "\.el$" "\.elc" f) f)

	   (let ((l
		  (with-temp-buffer
		    (insert-file-contents (expand-file-name f dir))
		    (loop while (< (point) (point-max)) collect (ignore-errors (read (current-buffer))))
		    )))
	     (loop for x in l when
		   (and (listp x) (memq (car x) *autoload-definition-list*))
		   collect (cadr x))
	     )
	   )
	  )
    )
  )

(defun emit-autoloads (top prefix filelist compiled)
  "produce an expression that when evaluated causes the definitions in specified filelist to be autoloaded

required arguments are
TOP the directory to autoload from
PREFIX a unique package name
FILELIST a list of (directory (relative filenames)) to scan
COMPILED t if autoload is to come from a compiled file
" 

  (let* ((top (if (string-match "/$" top) top (concat top "/")))
	 (prefix-autoload-alist (intern (concat prefix "-autoload-alist")))
	 (alldefs (list (loop with ret = nil
			      for x in (find-defs filelist compiled)
			      nconc
			      (loop for def in (cdr x) collect (list def (concat top (car x) ))
				    into ret
				    finally return ret)))))
    `(
      (setq ,prefix-autoload-alist (quote ,@alldefs))
      (mapc (lambda (x) (let ((fn (condition-case x (symbol-function  (car x)) (error nil)))) (and (null fn) (autoload (car x) (cadr x) nil t))))  ,prefix-autoload-alist)
      )
    )
  )

(defun find-files-1 (dir &optional match predicate)
  (loop with ret = nil
	for x in 
	(directory-files dir)
	when 
	(and 
	 (let ((z (file-name-nondirectory x)))
	   (not (or (string= z ".") (string= z ".."))))
  ; only apply predicate to files
	 (or (file-directory-p (concat dir "/" x)) (not predicate) (funcall predicate x dir)))
	nconc 
	(if (file-directory-p (concat dir "/" x)) (find-files-1 (concat dir "/" x) match predicate) 
	  (if (or (not match) (string-match match x)) (list (concat dir "/" x))))
	into ret
	finally return ret
	)
  )
; (find-files-1 "/src/emacs/lisp" ".el$")
; (find-files-1 "/src/emacs" ".el$")

(defun find-files (dir &optional full match predicate)
  "returns a list with car DIR and cdr the list of files under dir specified relatively
"
  (let ((default-directory dir))
    (list dir
	  (find-files-1 "." match predicate))
    )
  )

; (find-files "/src/emacs/lisp" nil ".el$") 
; (find-files "/src/emacs" nil ".el$") 


(defun find-config-files (dir full match)
  "special case find files only including valid configs for current platform and `emacs-version' "

  (let* (
	 (default-directory dir)
	 (sysname (getenv "OS"))
	 (hostname (or (getenv "HOSTNAME") (getenv "COMPUTERNAME")))
	 (common-pattern "common")
	 (os-pattern (format "os\\(/%s\\|$\\)" (if (string-match "Windows_NT" sysname) "W32" sysname)))
	 (host-pattern (format "hosts\\(/%s\\|$\\)" hostname))
	 (emacs-version-pattern (format "%d" emacs-major-version))
	 (emacs-minor-version-pattern (format "%d.%d" emacs-major-version emacs-minor-version))
	 )


    (find-files dir nil match
		'(lambda (x dir) 
		   (or
		   (string-match common-pattern dir)
		   (string-match os-pattern dir)
		   (string-match host-pattern dir)
		   (string-match emacs-version-pattern dir))
		   )
		)
    )
  )

; (setq foo (find-config-files "/src/emacs" nil ".el$") )

(defun make-config-autoloads (dir top prefix &optional compiled sourcepath)
  (let 
      ((file (concat prefix "-autoloads"))
       (autoloads (emit-autoloads (or top dir) prefix (find-config-files dir nil ".el$") compiled)))

    (write-region "" nil file nil 'quiet)

    (loop
     for x in autoloads
     do
     (write-region (with-output-to-string (pp x)) nil file t 'quiet)
     )

    (when sourcepath 
      (write-region (with-output-to-string (pp `(add-to-list 'find-function-source-path ,dir))) nil file t 'quiet)
      )

    file
    )
  )
; (make-config-autoloads "/src/emacs" nil "config" t)

(defun make-autoloads (dir top prefix &optional compiled sourcepath &rest filelist)

  (let*
      ((file (concat prefix "-autoloads"))
       (filelist (list dir filelist))
       (autoloads (emit-autoloads (or top dir) prefix (or filelist (find-files dir nil ".el$")) compiled)))

    (write-region "" nil file nil 'quiet)

    (loop
     for x in autoloads
     do
     (write-region (with-output-to-string (pp x)) nil file t 'quiet)
     )

    (when sourcepath 
      (write-region (with-output-to-string (pp `(add-to-list 'find-function-source-path ,dir))) nil file t 'quiet)
      )

    file
    )
  )

; (let ((default-directory "/src/emacs/lisp")) (make-autoloads "/src/emacs/lisp" nil "a"))
; (make-autoloads "/src/xz/site-lisp" "/usr/share/emacs/site-lisp/xz-4.0/" "xz" t t)
; (let ((default-directory "/z/el")) (make-autoloads  "/z/el" "/usr/share/site-lisp/z-1.0/" "z" t t "doom.el" "conf-mode.el" "camel.el" "outlook-helpers.el" "wget.el" "doclist.el" "collections.el"))

;; example 
;; emacs --batch --load="make-autoloads.el" --eval "(make-config-autoloads \"/src/emacs\" nil \"config\" t)"
