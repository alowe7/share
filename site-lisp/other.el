(put 'other 'rcsid
 "$Id$")

(require 'typesafe)
(require 'dired)
(require 'input)
(require 'roll)

(defvar *force-copy-other* nil "cfo confirms before clobbering unless set")

(defun other-lastline (&optional p) 
  (cond ((< p (point-max))
	 (goto-char p))
	(t
	 (goto-char (point-max))
	 (forward-line -1))
    )
  )


(defun other-next-line (arg)
  (interactive "p")
  (funcall 
   (cond ((eq major-mode 'dired-mode) 'dired-next-line)
	  (t 'next-line))
   arg)
  )

(defun other-revert-buffer ()
  (cond ((eq major-mode 'dired-mode) (revert-buffer))
	)
  )

(defvar *wide-screen* 1280 "if `display-pixel-width` is greater than this, assume you have two monitors")

(defun other-zero () 
" find the zero coordinate of the other screen, if there is one.  tolerates multiple monitors"
  (if (and (> (display-pixel-width) *wide-screen*)
  ; its a wide-screen
	   (not (> (frame-parameter nil 'left)  *wide-screen*) ))
  ; currently on screen 1; zero is on screen 2
      (1+ *wide-screen*)
    0
    )
  )
; (other-zero)

(defun other-width () 
  (let ((w (display-pixel-width)))
    (if (> w *wide-screen*)
	(/ w 2)
      w)
    )
  )
; (other-width)

(defun get-filename ()
  "if in dired mode, returns `dired-get-marked-files' 
else returns `buffer-file-name'
"
  (cond
   ((eq major-mode 'dired-mode) 
    (condition-case err (dired-get-marked-files) (error nil)))
   (t 
    (or (string* (thing-at-point 'filename)) (buffer-file-name)))
   )
  )

(defun cfo1 (f obd)
  (let
      ((target (and f (concat obd (file-name-nondirectory f)) )) 
       (force *force-copy-other*)
       force1
       bail)

    (if (or force
	    (not (file-exists-p target))

	    (let ((ret (y-or-n-q-p (format "file %s exists.  overwrite?" target) "!")))
	      
	      (cond
	       ((eq ret ?!) (setq force t))  ; remember force setting for the rest of this execution
	       ((eq ret ?q) (setq bail t))
	       (ret (setq force1 t)))

	      (or force force1)
	      )
	    )

	(cond ((file-directory-p f)
	       (shell-command (concat "cp -r \"" f "\" " obd)))
	      (t    
	       (copy-file f target (or force force1))
					; (revert-buffer nil t)
	       (save-window-excursion (other-window 1) (revert-buffer nil t))
	       ))
      (message "")
      )
    )
  )

(defun cfo () (interactive)
  (let* ((obd (save-window-excursion (other-window 1) default-directory))
	 (f (get-filename)))
    (cond 
     ((null f) (error "not looking at a filename") )
     ((string= default-directory obd)
      (message (format "error: other window is in same directory (%s)" obd)))
     ((listp f)
      (loop
       with force = nil
       with bail = nil
       when bail return (message "bail!") 
       for x in f
       do (cfo1 x obd))
      )
     )
    )
  )


(defun rfo1 (f obd)
  (let
      ((target (and f (concat obd (file-name-nondirectory f)) )) 
       (force *force-copy-other*)
       force1
       bail)

    (if (or (and (boundp 'force) force)
	    (not (file-exists-p target))

	    (let ((ret (y-or-n-q-p (format "file %s exists.  overwrite?" target) "!")))
	      (if (eq ret ?!) (setq force t))
	      (if (eq ret ?q) (setq bail t))
	      (if (eq ret ?y) (setq force1 t))
	      (or force force1)))

	(progn
	  (rename-file f target (or force force1))
	  (revert-buffer nil t)
	  (save-window-excursion (other-window 1) (revert-buffer nil t))
	  ))
    (message "")
    )
  )

(defun rfo () (interactive)
  (let* ((obd (save-window-excursion (other-window 1) default-directory))
	 (f (get-filename)))
    (cond 
     ((null f) (error "not looking at a filename") )
     ((string= default-directory obd)
      (message (format "error: other window is in same directory (%s)" obd)))
     ((listp f)
      (loop
       with force = nil
       with bail = nil
       when bail return (message "bail!") 
       for x in f
       do (rfo1 x obd))
      )
     )
    )
  )

(provide 'other)