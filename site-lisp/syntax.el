(put 'syntax 'rcsid
 "$Id$")

(defun modify-syntax (arg)
  (interactive "P")
  (let* ((c (if arg (read-char "modify syntax for char: ") (char-after)))
	 (p (if arg "to: " (format "modify syntax for char %c from %c to: " c (char-syntax c))))
	 (s (read-char p)))
    (modify-syntax-entry c (format "%c" s))
    )
  )
(global-set-key "," 'modify-syntax)

(defun char-syntax-1 (c)
  (interactive "schar: ")
  (let* (
	 (syntax-ref '(
		       (?   "whitespace")
		       (?.  "punctuation")
		       (?w   "word")
		       (?_   "symbol")
		       (?(   "open parenthesis")
			 (?)   "close parenthesis")
		       (?\"  "expression prefix")
		       (?\"  "string quote")
		       (?$   "paired delimiter")
		       (92 "escape") ; \\
		       (?/  "character quote")
		       (?<  "comment-start")
		       (?>  "comment-end")))
	 (char (cond ((integerp c) c) ((stringp c) (aref c 0))))
	 (s (and char (char-syntax char))))

    (and s 
	 (message (cadr (assoc s syntax-ref))))
    )
  )