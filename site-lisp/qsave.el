(put 'qsave 'rcsid
     "$Id$")

;; (c) alowe 1993-2010

;;  You are permitted to copy, modify and redistribute this software and associated documentation.
;;  You may not change this copyright notice, and it must be included in any copy made.

(require 'cl)

;; retain command output on the qsave property of the interned buffer name

(defstruct qsave-cell
  "qsave context cell" 
  contents label data point)

;; currently not enforced

(defvar *maximum-qsaved-output-depth* nil
  "if set, output search list is not to exceed this depth")

(defun qsaved-output (b &optional v)
 
  (let* ((a (intern (buffer-name b))))
    (if v
	(put a 'qsave v))
    (get a 'qsave))
  )

(defun qsaved-output-index (b &optional v)
  (with-current-buffer b
    (let* ((a (intern (buffer-name))))
      (if v
	  (put a 'qsaved-index v))
      (get a 'qsaved-index))
    )
  )

(defun prune-search (&optional n b)
  " prune to optional depth N the search list associated with BUFFER.
default depth is 0, default buffer is current-buffer. 
updates window to reflect new top of list.
returns newly current cell data, if any
"
  (interactive "ndepth of saved list: ")
  (with-current-buffer (or b (current-buffer))
    (let* ((a (intern (buffer-name)))
	   (n (or n 0))
	   (v (get a 'qsave))
	   (len (length v)))
      (prog1
	  (cond ((= n 0) 
		 (progn 
		   (put a 'qsave nil)
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   (setq buffer-read-only t)
		   (put a 'qsaved-index 0)
		   (setq mode-line-buffer-identification nil)
		   (setq mode-line-process (format " %d/%d" (length v) (length v)))
		   ))
		((> len n)
		 (let ((m (- len n))
		       ss)
		   (while v (push (pop v) ss))
		   (while (> m 0) (progn
				    (pop ss)
				    (setq m (1- m))))
		   (while ss (push (pop ss) v))
		   (let* ((j (length v))
			  (x (nth (1- j) v)))
		     (erase-buffer)
		     (insert (qsave-cell-contents x))
		     (setq mode-line-buffer-identification (pp (qsave-cell-label x) t))
		     (set-window-start (display-buffer (current-buffer)) 1)
		     (setq mode-line-process (format " %d/%d" j j))
		     (put a 'qsaved-index j)
		     (put a 'qsave v)
		     (qsave-cell-data x)
		     )
		   )
		 )
		)
	(force-mode-line-update)
	)
      )
    )
  )
(fset 'qsave-prune-search 'prune-search)
(fset 'prune-qsave 'prune-search)

;; the qsave property of the interned buffer name holds a stack
;; of previous queries

(defun qsave-search (b l &optional d)
  "push contents of BUFFER and associated LABEL along with optional DATA
into internal stack"
  (with-current-buffer b

    (let* ((p (point))
	   (a (intern (buffer-name)))
	   (v (get a 'qsave))
	   (x (make-qsave-cell :contents (buffer-substring (point-min) (point-max))
			       :label l
			       :data d
			       :point p
			       )))

      (put a 'qsave (push x v))

      ;; now looking at most recent one
      (put a 'qsaved-index 0)
      (setq mode-line-process (format " %d/%d" (length v) (length v)))
      ;; ensure mode-line-buffer-identification is a string
      (setq mode-line-buffer-identification (pp l t))
      )
    )
  )


(defun previous-qsave-search (&optional b)
  " move to previous search context in the stack of contexts 
associated with optional buffer B (default `current-buffer')
returns data on cell, if any.
"
  (interactive)
  (let* ((a (intern (buffer-name (or b (current-buffer)))))
	 (v (get a 'qsave))
	 (i (get a 'qsaved-index))
	 (len (length v)))
    (if (not v) 
	(progn 
	  (message "no saved command output")
	  nil)
      (if (< i (1- len))
	  (let* ((j (1+ i))
		 (x (nth j v))
		 (save-buffer-read-only buffer-read-only)
		 (buffer-read-only nil))
	    (erase-buffer)
	    (insert (qsave-cell-contents x))
	    (set-buffer-modified-p nil)
	    (setq buffer-read-only save-buffer-read-only)
	    (goto-char (qsave-cell-point x))
	    (setq mode-line-buffer-identification (pp (qsave-cell-label x) t))
	    (set-window-start (display-buffer (current-buffer)) 1)
	    (setq mode-line-process (format " %d/%d" (- len j) len))
	    (put a 'qsaved-index j)
	    (qsave-cell-data x)
	    )
	(progn
	  (message "bottom of saved command output list")
	  nil)
	)
      )
    )
  )

(defun next-qsave-search  (&optional b)
  " move to next search context in the stack of contexts 
associated with optional buffer B (default `current-buffer')
returns data on cell, if any.
"
  (interactive)
  (let* ((a (intern (buffer-name (or b (current-buffer)))))
	 (v (get a 'qsave))
	 (i (get a 'qsaved-index))
	 (len (length v)))
    (if (not v) 
	(progn 
	  (message "no saved command output")
	  nil)
      (if (> i 0)
	  (let* ((j (1- i))
		 (x (nth j v))
		 (save-buffer-read-only buffer-read-only)
		 (buffer-read-only nil))
	    (erase-buffer)
	    (insert (qsave-cell-contents x))
	    (set-buffer-modified-p nil)
	    (setq buffer-read-only save-buffer-read-only)
	    (goto-char (qsave-cell-point x))
	    (setq mode-line-buffer-identification (pp (qsave-cell-label x) t))
	    (set-window-start (display-buffer (current-buffer)) 1)
	    (setq mode-line-process (format " %d/%d" (- len j) len))
	    (put a 'qsaved-index j)
	    (qsave-cell-data x)
	    )
	(progn
	  (message "top of saved command output list")
	  nil)
	)
      )
    )
  )

;; minor mode for qsave -- used for readonly buffers, where we can clobber the "p" and "n" keys.

(defvar *qsave-mode* nil)
(make-variable-buffer-local '*qsave-mode*)

(defvar *saved-local-key-bindings* nil)
(make-variable-buffer-local '*saved-local-key-bindings*)

(defun roll-qsave () (interactive) (previous-qsave-search (current-buffer)))
(defun roll-qsave-1 () (interactive) (next-qsave-search (current-buffer)))

;  (condition-case x (cd (previous-qsave-search (current-buffer))) (error nil))

(defvar *qsave-mode-bindings* '(("p" roll-qsave) ("n" roll-qsave-1) ("d" prune-qsave)))

(defun qsave-mode (&optional arg)
  "toggle qsave mode.  with optional arg enter qsave mode iff arg > 0"
  (let ((prev-qsave-mode *qsave-mode*))

    (setq *qsave-mode*
	  (if (null arg) (not *qsave-mode*)
	    (> (prefix-numeric-value arg) 0)))

    (if *qsave-mode*
					; enter qsave mode

	(unless prev-qsave-mode		; unless already in it

					; save prior local key bindings, if any
	  (setq *saved-local-key-bindings*
		(loop for x in *qsave-mode-bindings* collect (list (car x) (local-key-binding (car x)))))

					; apply qsave local key bindings
	  (loop for x in *qsave-mode-bindings* do (apply 'local-set-key x))

	  )
					; leave qsave mode
      (progn
	(loop for x in *saved-local-key-bindings* when (cadr x) do (local-set-key (car x) (cadr x)))

	(force-mode-line-update)
	)
      )
    )
  )

(add-to-list 'minor-mode-alist '(qsave-mode "qsave"))

;; advice for using:
;; (add-hook 'qsave-minor-mode-after-search-hook 
;;   '(lambda () (qsave-search buffer))
;;   " save output of each qsave-minor-mode search on a stack for retrieval")
;; 
;; (add-hook 'qsave-minor-mode-init-hook '(lambda () 
;; (define-key qsave-minor-mode-mode-map "p" 'previous-qsave-minor-mode-search)
;; (define-key qsave-minor-mode-mode-map "n" 'next-qsave-minor-mode-search)
;; ))
;; 

(provide 'qsave)
