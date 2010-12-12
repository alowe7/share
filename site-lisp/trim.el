(put 'trim 'rcsid
 "$Id$")

(require 'long-comment)

(defun trim-trailing-white-space (&optional s) (interactive)
  " trim trailing white space from STRING"
  (if (interactive-p)
      (while (search-forward "[ 	]*$" nil t)
	(replace-match "" nil t))
    (and s 
	 (let* ((p (string-match "[ 	]$" s)))
	   (substring s 0 p)))))

(defun trim-leading-white-space (&optional s) 
  " trim leading white space.
when called from a program, and optional STRING is specified, trim white space from that."
  (interactive)
  (if (interactive-p)
      (save-excursion
	(while (re-search-forward "^[ 	]+" nil t)
	  (replace-match "" nil t))
	)
    (and s (replace-regexp-in-string "^[\t\n ]+" "" s))
    )
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
  (while (search-forward "[ ]*$" nil t)
    (replace-match "" nil t))
  )

(defun trim-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[ \t]*$" end t)
      (replace-match "" nil nil))
    )
  )

(defun trim-blank-lines-region (beg end)
  "trim blank lines from region.
"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward  "[\n]+" end t)
      (replace-match "\n" nil nil))
    )
  )

(defun trim-blank-lines (&optional s)
  "if interactive, `trim-blank-lines-region'
else from a program, trim blank lines and return optional STRING
"
  (interactive "P")
  (cond 
   ((string* s)
    (replace-regexp-in-string "[\n]+" "\n" s))
   (t s)
   )
  )

(/*
 (assert (string= (trim-blank-lines "asdf
sadf

sadf	asdf") "asdf
sadf
sadf	asdf")
	 )
 )

(provide 'trim)
