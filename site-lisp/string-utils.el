(put 'string-utils 'rcsid
 "$Id$")

(defun string^ (s pat)
  "perform exclusive or of STRING with PAT."
  (if (string-match pat s)
      (replace-regexp-in-string pat "" s)
    (concat s pat))
  )

(defun string& (s pat)
  "append STRING by PAT, unless already there."
  (cond
   ((string-match (concat pat "$") s) s)
   (t (concat s pat)))
  )

;; xxx move to trim.el
(defun tr (target trmap)
  "replace chars in TARGET according to alist MAP
where map is an alist of the form: ((char1 string1) (char2 string2))

target may be a string or a buffer; 

returns result;
"

  (cond 
   ((stringp target)
    (let (prev)
      (apply 'concat 
	     (remove nil 
		     (loop for x across target 
			   collect
			   (prog1
			       (if (and 
				    (assoc x trmap) 
				    (not (and prev (char-equal prev ?\\ )))
				    )
				   (cadr (assoc x trmap))
				 (format "%c" x))
			     (setq prev x)
			     )
			   )
		     )
	     )
      ))
   ((buffer-live-p target)
    (with-current-buffer target
      (loop for x in trmap
	    do
	    (goto-char (point-min))
	    (while (search-forward (format "%c" (car x)) nil t)
	      (replace-match (cadr x) nil t))
	    )
      (buffer-string)
      ))
   (t 
    (error "invalid target for `tr'"))
   )
  )
; (insert (tr "foo\\.bar" '((?* "%") (?. "_"))))


(provide 'string-utils)
