(put 'byte-compile-directory 'rcsid 
 "$Id$")

(defvar *share* (expand-file-name (or (getenv "SHARE") "/usr/share/emacs")))

(mapc (lambda (x) (add-to-list 'load-path x))
      (nconc (directory-files (concat *share* "/site-lisp") t "^[a-zA-Z]")
	     (list "." (concat *share* "/site-lisp"))))

(setq byte-compile-warnings '(not cl-functions free-vars unresolved obsolete))
; (mapc 'byte-compile-file (directory-files "." nil "\.el$"))
