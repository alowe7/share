(put 'trim 'rcsid
 "$Id$")


(defun trim-trailing-white-space (&optional s) (interactive)
  " trim trailing white space from STRING"
  (if (interactive-p)
      (replace-regexp "[ 	]*$" "")
    (and s 
	 (let* ((p (string-match "[ 	]$" s)))
	   (substring s 0 p)))))

(defun trim-leading-white-space (&optional s) 
  " trim leading white space from STRING"
  (interactive)
  (if (interactive-p)
      (save-excursion
	(replace-regexp "^[ 	]*" "")
	)
    (and s 
	 (replace-regexp-in-string "^[ 	]+" "" 
			    (replace-regexp-in-string "
[ 	]+" "
" s)
			    )))
  )

(defun trim-white-space (&optional s) 
	"trim white-space in string.  when called interactively, trims region."
	(interactive)
  (if (interactive-p)
			(save-restriction
			 (narrow-to-region (point) (mark))
			 (goto-char (point-min))
			 (call-interactively 'trim-leading-white-space)
			 (goto-char (point-min))
			 (call-interactively 'trim-trailing-white-space))
    (if (> (length s) 0)
				(trim-trailing-white-space (trim-leading-white-space s))
      s)
    )
  )
(fset 'trim 'trim-white-space)

(defun trim-buffer ()
  (replace-regexp "[ ]*$" "" nil)
  )

(defun trim-region (begin end)
	(interactive "r")
	(save-excursion
		(narrow-to-region begin end)
		(replace-regexp "[ ]*$" "" nil)
		(widen)
		)
	)

(defun trim-blank-lines (&optional s)
  "trim blank lines from region.
"
  (interactive "P")
  (if (and (interactive-p) (null s))
      (let ((s (buffer-substring (point) (mark))))
	(delete-region (point) (mark))
	(insert
	 (replace-regexp-in-string "^[	 ]*
" "" s)))

    (replace-regexp-in-string "^[	 ]*
" "" s)
    )
  )

(provide 'trim)
