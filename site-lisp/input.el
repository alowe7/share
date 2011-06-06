(put 'input 'rcsid 
 "$Id$")

; tbd use read-event
(defun read-char-p ()
  (condition-case err
      (read-char)
    (error 
     last-input-event)
    )
  )
;(read-char-p)

(defun char-equal-p (c1 c2)
  (condition-case err
      (equal c1 c2)
    (error 
     last-input-event)
    )
  )
;(char-equal-p ?\M-n 134217838)
;(char-equal-p ?\M-n ?a)
;(char-equal-p "key" "key")

(defun y-or-n-*-p (prompt &optional chars &rest args)
  "display PROMPT and read characters.
returns t for y, nil for n (as in `y-or-n-p')
with optional string CHARS, also matches specified characters.
"
  (interactive)

  (let (prompt-includes-help)

    (catch 'done
      (while t
	(apply 'message (cons prompt args))
	(let ((c (read-char-p)) x)
	  (message "")
	  (cond 
	   ((or (null c) (char-equal-p c ?n))
	    (throw 'done nil))
	   ((char-equal-p c ?y)
	    (throw 'done t))
	   (chars
	    (dotimes (x (length chars)) 
	      (let ((c2 (aref chars x))) 
		(and (char-equal-p c c2) (throw 'done c2)))))
	   )

	  (unless prompt-includes-help
	    (setq prompt (format "%s (y to continue, n for next%s): " prompt 
				 (if (string* chars) (concat ", or [" chars "]") "")
				 )
		  prompt-includes-help t))
	   )
	  )
	)
      )
  )

; (y-or-n-*-p "test: ")
; (y-or-n-*-p "test (%s): " "abc" "foo")

(defun remove-text-properties-from-string (str)
  "returns a copy of STRING with text properties removed
"
  (with-temp-buffer 
    (insert str)
    (set-text-properties (point-min) (point-max) nil)
    (buffer-string))
  )

(defun y-or-n-q-p (prompt &optional chars &rest args)
  "display PROMPT and read characters.
returns t for y, nil for n ?q for q, else loop
with optional string CHARS, also matches specified characters.
"
  (interactive)

  (let (prompt-includes-help)

    (catch 'done
      (while t
	(apply 'message (cons (remove-text-properties-from-string prompt) args))

	(let ((c (read-char-p)) x)
	  (cond 
	   ((char-equal-p c ?q) (throw 'done ?q))
	   ((char-equal-p c ?y) (throw 'done t))
	   ((char-equal-p c ?n) (throw 'done nil))
	   (chars
	    (dotimes (x (length chars)) 
	      (let ((c2 (aref chars x))) 
		(and (char-equal-p c c2) (throw 'done c2)))))
	   )

	  (unless prompt-includes-help
	    (setq prompt (format "%s (press y, n, or q%s): " prompt
;;			tbd handle vector of events using something like:
;; 	 (if (vectorp chars) (key-description ...)
				 (if (string* chars)
				     (concat ", or one of [" chars "]") "")
				 )
		  prompt-includes-help t))
	  )
	)
      )
    )
  )

; (y-or-n-q-p "foo (%s)?" "xz" "bar")

(provide 'input)
