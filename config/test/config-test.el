;; verify all dependencies on load-path
(let (
      (load-path
       (list
	(expand-file-name "site-lisp" (getenv "emacs_dir"))
	(expand-file-name "site-lisp" "../..")
	(expand-file-name "config" "../..")
	(expand-file-name "lisp" (getenv "emacs_dir"))
	(expand-file-name "lisp/emacs-lisp" (getenv "emacs_dir"))
	))
      )
  (load-library "config")
  )
