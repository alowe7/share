;; $Id$

(require 'typesafe)
(require 'fapropos)

; todo combine buff.el with buffers.el

(defun list-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (interactive "P")
  (display-buffer (list-buffers-noselect files-only)))

(defun list-buffers-noselect (&optional files-only buffer-list-fn buffer-list-name)
  "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only."
  (let ((old-buffer (current-buffer))
	(standard-output standard-output)
	desired-point)
    (with-current-buffer 
	(get-buffer-create (or buffer-list-name "*Buffer List*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq standard-output (current-buffer))
      (princ "\
 MR Buffer           Size  Mode         File
 -- ------           ----  ----         ----
")
      ;; Record the column where buffer names start.
      (setq Buffer-menu-buffer-column 4)
      (let ((bl (funcall (or buffer-list-fn 'buffer-list))))
	(while bl
	  (let* ((buffer (car bl))
		 (name (buffer-name buffer))
		 (file (buffer-file-name buffer))
		 this-buffer-line-start
		 this-buffer-read-only
		 this-buffer-size
		 this-buffer-mode-name
		 this-buffer-directory)
	    (with-current-buffer buffer
	      (setq this-buffer-read-only buffer-read-only)
	      (setq this-buffer-size (buffer-size))
	      (setq this-buffer-mode-name
		    (if (eq buffer standard-output)
			"Buffer Menu" mode-name))
	      (or file
		  ;; No visited file.  Check local value of
		  ;; list-buffers-directory.
		  (if (and (boundp 'list-buffers-directory)
			   list-buffers-directory)
		      (setq this-buffer-directory list-buffers-directory))))
	    (cond
	     ;; Don't mention internal buffers.
	     ((string= (substring name 0 1) " "))
	     ;; Maybe don't mention buffers without files.
	     ((and files-only (not file)))
	     ;; Otherwise output info.
	     (t
	      (setq this-buffer-line-start (point))
	      ;; Identify current buffer.
	      (if (eq buffer old-buffer)
		  (progn
		    (setq desired-point (point))
		    (princ "."))
		(princ " "))
	      ;; Identify modified buffers.
	      (princ (if (buffer-modified-p buffer) "*" " "))
	      ;; Handle readonly status.  The output buffer is special
	      ;; cased to appear readonly; it is actually made so at a later
	      ;; date.
	      (princ (if (or (eq buffer standard-output)
			     this-buffer-read-only)
			 "% "
		       "  "))
	      (princ name)
	      ;; Put the buffer name into a text property
	      ;; so we don't have to extract it from the text.
	      ;; This way we avoid problems with unusual buffer names.
	      (setq this-buffer-line-start
		    (+ this-buffer-line-start Buffer-menu-buffer-column))
	      (let ((name-end (point)))
		(indent-to 17 2)
		(put-text-property this-buffer-line-start name-end
				   'buffer-name name)
		(put-text-property this-buffer-line-start name-end
				   'mouse-face 'highlight))
	      (let (size
		    mode
		    (excess (- (current-column) 17)))
		(setq size (format "%8d" this-buffer-size))
		;; Ack -- if looking at the *Buffer List* buffer,
		;; always use "Buffer Menu" mode.  Otherwise the
		;; first time the buffer is created, the mode will be wrong.
		(setq mode this-buffer-mode-name)
		(while (and (> excess 0) (= (aref size 0) ?\ ))
		  (setq size (substring size 1))
		  (setq excess (1- excess)))
		(princ size)
		(indent-to 27 1)
		(princ mode))
	      (indent-to 40 1)
	      (or file (setq file this-buffer-directory))
	      (if file
		  (princ file))
	      (princ "\n"))))
	  (setq bl (cdr bl))))
      (Buffer-menu-mode)
      ;; DESIRED-POINT doesn't have to be set; it is not when the
      ;; current buffer is not displayed for some reason.
       (goto-char (or desired-point (point-min)))
      (current-buffer))))


; (display-buffer (list-buffers-noselect nil '(lambda () (collect-buffers-mode 'dired-mode))))
; (display-buffer (list-buffers-noselect nil '(lambda () (collect-buffers-named "buf"))))
; (display-buffer (list-buffers-noselect nil '(lambda () (collect-buffers-mode 'shell-mode))))


(defun list-buffers-helper (pred bn)
    (when (buffer-live-p (get-buffer bn))
      (condition-case x (kill-buffer bn) (error nil)))
    (display-buffer (list-buffers-noselect nil pred bn))
    )

(defun list-buffers-modified ()
  (interactive)
  (list-buffers-helper 'collect-buffers-modified "*Buffer List <modified>*")
  )

(defun list-buffers-not-modified ()
  (interactive)
  (list-buffers-helper 'collect-buffers-not-modified "*Buffer List <not modified>*")
  )

(defun get-mode ()
  (let ((m (completing-read* (format "mode (%s): " major-mode)
			    (mapcar '(lambda (x) 
				       (cons
					(format "%s" x) x))
				    (functions-like "-mode$")
				    ))))
    (or (and m (intern m)) major-mode)
    )
  )

(defun list-buffers-mode (mode)
  (interactive 
   (list (get-mode)))

  (list-buffers-helper
   `(lambda () (collect-buffers-mode (quote ,mode)))
   (format "*Buffer List <%s>*" mode))
  )

(defun list-buffers-in (pat)
  (interactive "sbuffers with files matching: ")
  (list-buffers-helper 
   `(lambda () (collect-buffers-in (quote ,pat)))
   (format "*Buffer List <%s>*" pat))
  )

(defun list-buffers-named (pat)
  (interactive "sbuffers with names matching: ")

  (list-buffers-helper 
   `(lambda () (collect-buffers-named ,pat))
   (format "*Buffer List <%s>*" pat))
  )

(defun list-buffers-with (pat)
  (interactive (list (read-string* "buffers with contents matching (%s): " (indicated-word))))
  ; todo: keep track of line numbers, transient advise buffer list visit to jump to hit

  (list-buffers-helper 
   `(lambda () (collect-buffers-with (quote ,pat)))
   (format "*Buffer List With \'%s\'*" pat)
   )
  )

(provide 'buff)