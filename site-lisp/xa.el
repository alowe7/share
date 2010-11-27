(put 'xa 'rcsid
 "$Id$")

(define-derived-mode xa-mode fundamental-mode "xa" "")

(defun xa (&optional prompt initial-input buffer cancel-message)
  "switch to a temp buffer to edit an entry.
giving optional PROMPT
starting out with INITIAL-INPUT
using BUFFER (a buffer or buffer name)
if CANCEL-MESSAGE is set, puts up a message when cancelled
return the bufferstring

 inside the edit buffer, use C-c C-c to exit,  C-c C-u to cancel
"

  (condition-case v
      (let ((b
	     (cond 
	      ((not buffer) (get-buffer-create "*edit new entry*"))
	      ((stringp buffer) (get-buffer-create buffer))
	      (t buffer))
	     ) 
	    s)
	(save-excursion
	  (if (catch 'done
		(switch-to-buffer b)
		(xa-mode)
		(if prompt (setq mode-line-buffer-identification prompt))
		(if initial-input (progn (insert initial-input) (goto-char (point-min))))
  ; todo make this a custom sub mode
		(local-set-key "\C-c\C-c" 'xa-done)
		(local-set-key "\C-c\C-u" '(lambda () (interactive) (y-or-n-p "are you sure? ") (throw 'done  t)))
		(setq fill-column (max (- (frame-width) 15) 70))
		(message "C-c C-c to exit	 C-c C-u to cancel")
		(sit-for 1)
		(recursive-edit))
	      (if cancel-message (progn 
				   (message "cancelled")
				   (sit-for 0 500)
				   (message "")))
	    (setq s (buffer-string))
	    ))
	(unless buffer (kill-buffer b)) ; kill b only if we created it
	s)
    (error nil)
    )
  )

(defun xa-done ()
  (interactive)
  (kill-new (buffer-string)) ; save as a kill in case caller loses
  (throw 'done nil)
  )

(provide 'xa)