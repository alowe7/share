(put 'file-association 'rcsid
 "$Id$")

(defvar file-assoc-list nil
  "assoc mapping of downcased file extensions to handlers")

(defun add-file-association (type handler)
  "add assoc mapping of downcased file extension to handler.
see `file-assoc-list'"
  (or (assoc type file-assoc-list)
      (push
       (cons type handler)
       file-assoc-list))
  )


(defun file-association-1 (f &optional notrim)
  "find command associated with filetype of specified file"
  (interactive "sFile: ")
  (cdr (assoc (file-name-extension f) file-assoc-list))
  )

(defun file-association (f &optional notrim)
  "find command associated with filetype of specified file"
  (interactive "sFile: ")
  (let ((cmd (perl-command "plassoc" (concat "." (file-name-extension f)) )))
    ;; sometimes command line args are appended to ocmd.
    ;; we usually want just the executable
    (if cmd 
	(if notrim cmd
	  (car (catlist cmd ?\"))
	  )
      )
    )
  )

;(add-file-association "lnk" 'lnk-view)
;(add-file-association "tar" 'tar-view)
;(add-file-association "zip" 'zip-view)
;(add-file-association "html" 'html-view)
;(add-file-association "htm" 'html-view)

(provide 'file-association)
