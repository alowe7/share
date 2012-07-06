(put 'buffers 'rcsid 
 "$Id$")

(require 'zap)
(require 'cl)

(defun real-buffer-list (&optional arg)
  "returns `buffer-list' excluding killed buffers & those with names beginning with a space
with optional ARG, returns in reverse order
"
  (let* ((rbl (buffer-list))
	 (bl (append (cdr rbl) (list (car rbl))))
	 bn val x)
    (dolist (x (if arg bl (reverse bl)))
      (setq bn (buffer-name x))
  ;skip killed buffers & those whose name begins with a space
      (and bn (> (length bn) 0) (not (eq ?\  (aref bn 0))) (push x val)))
    val))

(defun* buffer-list-2 (&key mode named in modified notmodified withpat)
  ; (assert (or (not (or modified notmodified)) (not (and modified notmodified))))

  (loop for x being the buffers
	when
	(and (or (not mode)
		 (eq (progn (set-buffer x) major-mode) mode))
	     (or (not named)
		 (string-match named (buffer-name x)))
	     (or (not in)
		 (let ((d (if (buffer-file-name x) (buffer-file-name x) (with-current-buffer  x default-directory))))
		   (and d (string-match in d))))
	     (or (not modified)
		 (buffer-modified-p x))
	     (or (not notmodified)
		 (not (buffer-modified-p x)))
	     (or (not withpat)
		 (string-match withpat (with-current-buffer  x (buffer-string)))))
	collect x)
  )

;; walk mru list of buffers

(defun collect-buffers (mode) 
  (let ((l (loop for x being the buffers
		 if (eq mode (progn (set-buffer x) major-mode))
		 collect x)))
    (and l (append (cdr l) (list (car l))))
    )
  )


(defun collect-buffers-mode (mode)
  (interactive "Smode: ")
  "returns a list of buffers with specified MODE
when called interactively, displays a pretty list"
  (let ((l 
	 (loop
	  for x being the buffers 
	  if (eq (progn (set-buffer x) major-mode) mode) 
	  collect x)))
    )
  )

(defun collect-buffers-named (pat)
  "list buffers with names matching PAT"
  (interactive "spat: ")
  (loop for x in (real-buffer-list nil)
	when (string-match pat (buffer-name x))
	collect x )
  )

(defun collect-buffers-with (pat)
"list buffers with contents matching PAT"
  (loop for x in (real-buffer-list nil)
	when (string-match pat (with-current-buffer x (buffer-string)))
	collect x )
  )

(defun collect-buffers-with-mode (pat mode)
  "list buffers with contents matching PAT and `major-mode' matching MODE"
  (loop for x in (real-buffer-list nil)
	when (with-current-buffer x
	       (and (eq mode major-mode)
		    (string-match pat (buffer-string)))
	       )
	collect x)
  )

(defun collect-buffers-in (pat)
  "list buffers with files matching PAT" 
  (loop for x in (real-buffer-list nil)
	when (let ((d (if (buffer-file-name x) (buffer-file-name x) (with-current-buffer x default-directory))))
	       (and d (string-match pat d)))
	collect x )
  )

(defun collect-buffers-modified (&optional arg)
  "list buffers that are modified.  with optional ARG, restrict to only buffers with files"
  (loop for x in (real-buffer-list) when (and (buffer-modified-p x) (or (not arg) (buffer-file-name x)) ) collect x)
  )

(defun collect-buffers-not-modified (&optional arg)
  "list buffers that are not modified.  with optional ARG, restrict to only buffers with files"
  (loop for x in (real-buffer-list) when (and (not (buffer-modified-p x)) (or (not arg) (buffer-file-name x)) ) collect x)
  )

(defun collect-buffers-no-files (&optional modified)
  (loop for x being the buffers
	when (and 
	      (not (progn (set-buffer x) (buffer-file-name)))
	      (or (not modified) (buffer-modified-p x)))
	collect (buffer-name)))

(defun find-in-buffers (s &optional buffer-list)
  "find string s in any buffer.  returns a list of matching buffers"
  (loop for x being the buffers 
	if (with-current-buffer x
	     (goto-char (point-min))
	     (search-forward s nil t))
	collect x))

(defun kill-buffers-mode (mode)
  "kill all buffers in mode"
  (interactive (list (intern (completing-read "mode: "  (mapcar 'list (symbols-like "-mode$" t))))))
  (loop for x in (collect-buffers-mode mode)
	do
	(kill-buffer x))
  )

(defun kill-buffers-not-modified ()
  "kill all buffers in mode"
  (interactive)
  (and (y-or-n-p "kill-buffers-not-modified.  are you sure? ")
       (loop for x in (collect-buffers-not-modified)
	     do
	     (kill-buffer x)))
  )

(provide 'buffers)
