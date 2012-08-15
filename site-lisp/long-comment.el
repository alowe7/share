(put 'long-comment 'rcsid 
 "$Id$")
(provide 'long-comment)

(defmacro long-comment (&rest args) "long comment: ignore all arguments unevaluated" nil)


(defalias '/* (symbol-function 'long-comment))

(defvar long-comment-beginning "(/*
")
(defvar long-comment-end "
  */)
")
(defvar long-comment-continue "  * ")

(defun long-comment-region (beg end)
  "add long-comment around region
"
  (interactive "r")

  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match long-comment-continue nil nil))


    (goto-char (point-min))
    (insert long-comment-beginning)
    (goto-char (point-max))
    (insert long-comment-end)

    (widen)
    )
  )


; this is kind of dumb
(defun long-uncomment-region (beg end)
  "remove long-comment around region
"
  (interactive "r")

  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
  
    (while (not (eobp))
      (progn
	(beginning-of-line)
	(cond 
	 ((looking-at "([\s ]*/\\*[\s ]*")
	  (replace-match ""))
	 ((looking-at "[\s ]*\\*/)")
	  (replace-match ""))
	 ((looking-at "[\s ]*[\\*]+[\s ]*")
	  (replace-match ""))
	 )
	(forward-line 1)
	)
      )
    (widen)    
    )
  )