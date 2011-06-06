(put 'zap 'rcsid 
 "$Id$")

;;; todo -- use (get-buffer-create (generate-new-buffer-name bname))

;; needed only to get arglist of subrs
(require 'advice)
(require 'arity)


(defun zap-buffer (bname &optional postop preop)
  "set buffer BUFFER, create if necessary, erase contents if necessary
with optional POSTOP, evaluates POSTOP in context of buffer after creation
with optional PREOP, evaluates PREOP before calling `get-buffer-create'
"
  (interactive "Bbuffer: ")
  (let (v)
    (and preop (eval-p preop))
  ; if buffer existed and was read only, there's probably little 
  ; utility in keeping it that way, after zapping it!
    (and (get-buffer bname) (kill-buffer bname))
    (setq v (set-buffer (get-buffer-create bname)))
    (and postop (eval-p postop))
    v)
  )

(defun zap-buffer-0 (bname &optional postop preop)
  "set buffer BUFFER, create if necessary, erase contents if necessary
with optional POSTOP, evaluates POSTOP in context of buffer after creation
with optional PREOP, evaluates PREOP before calling `get-buffer-create'
"
  (interactive "Bbuffer: ")
  (let (v)
    (and preop (eval preop))
  ; if buffer existed and was read only, there's probably little 
  ; utility in keeping it that way, after zapping it!
    (and (get-buffer bname) (kill-buffer bname))
    (setq v (get-buffer-create bname))
    (and postop (eval postop))
    v)
  )


(defun zap-buffer-1 (name)
  "return existing buffer NAME, create if necessary, erase contents if necessary.
see `get-buffer-create'
takes no action and returns nil if buffer exists and is read-only
"
  (interactive "Bbuffer name: ")

  (let ((b (get-buffer name)))
    (if b (with-current-buffer  b 
			  (condition-case err
			      (progn
				(erase-buffer)
				b)
			    (buffer-read-only 
  ;			     (error " buffer exists: %s" name)
			     nil)
			    ))
      (get-buffer-create name)
      )
    )
  )
; (zap-buffer-1 "foo")

(defun zap-buffer-2 (name &optional postop)
  "return existing buffer NAME, create if necessary, erase contents if necessary.
see `get-buffer-create'
toggles `buffer-read-only' erases buffer if buffer exists and is read-only

note: leaves focus in the newly created buffer.
"
  (interactive "Bbuffer name: ")

  (let ((b (or (get-buffer name))))
    (if b (progn
	    (set-buffer b) 
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    b)
      (progn
	(set-buffer (get-buffer-create name)))
      )
    (and postop (eval-p postop))
    )
  )

(provide 'zap)
