(put 'collect-directories 'rcsid
 "$Id$")

(require 'cl)

(defun collect-directories (d)
  "collect closure of directory DIR and all subdirectories
"

  (let ((default-directory d))
    (nconc (list d)
	   (loop
	    for f in (directory-files d)  
	    with l = nil
	    when (and (not (string= f ".")) (not (string= f "..")) (file-directory-p f))
	    nconc (collect-directories (expand-file-name f d)) into l
	    finally return l
	    )
	   )
    )
  )

; (setq x (collect-directories (expand-file-name "lisp/cedet" source-directory)))

(provide 'collect-directories)
