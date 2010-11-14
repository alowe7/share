(put 'input 'rcsid 
 "$Id: input.el 890 2010-10-04 03:34:24Z svn $")

(defun read-char-p ()
  (condition-case err
      (read-char)
    (error 
     last-input-event)
    )
  )
;(read-char-p)

(defun y-or-n-*-p (prompt &optional chars &rest args)
  "display PROMPT and read characters.
returns t for y, nil for n (as in `y-or-n-p')
with optional string CHARS, also matches specified characters.
"
  (interactive)
  (catch 'done
    (while t
      (apply 'message (cons prompt args))
      (let ((c (read-char-p)) x)
	(message "")
	(cond 
	 ((or (null c) (char-equal c ?n))
	  (throw 'done nil))
	 ((char-equal c ?y)
	  (throw 'done t))
	 (chars
	  (dotimes (x (length chars)) 
	    (let ((c2 (aref chars x))) 
	      (and (char-equal c c2) (throw 'done c2)))))
	 (t (setq prompt (format "%s (y [%s] to continue, n for next): " prompt 
				 (string* chars "")
				 )))
	 )
	)
      )
    )
  )

; (y-or-n-*-p "test: ")

(defun remove-text-properties-from-string (str)
  "returns a copy of STRING with text properties removed
"
  (with-temp-buffer 
    (insert str)
    (remove-text-properties (point-min) (point-max) '(face))
    (buffer-string))
  )

(defun y-or-n-q-p (prompt &optional chars &rest args)
  "display PROMPT and read characters.
returns t for y, nil for n ?q for q, else loop
with optional string CHARS, also matches specified characters.
"
  (interactive)

  (catch 'done
    (while t
      (apply 'message (cons (remove-text-properties-from-string prompt) args))
      (let ((c (read-char-p)) x)
	(cond 
	 ((char-equal c ?q) (throw 'done ?q))
	 ((char-equal c ?y) (throw 'done t))
	 ((char-equal c ?n) (throw 'done nil))
	 (chars
	  (dotimes (x (length chars)) 
	    (let ((c2 (aref chars x))) 
	      (and (char-equal c c2) (throw 'done c2)))))
	 (t (setq prompt (format "%s (y to continue, n for next, q to quit loop): " prompt)))
	 )
	)
      )
    )
  )

; (y-or-n-q-p "foo (%s)?" "xyz" "bar")

(provide 'input)
