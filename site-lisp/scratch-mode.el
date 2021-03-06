(put 'scratch-mode 'rcsid
 "$Id$")

(require 'ctl-ret)
(require 'buff)
(require 'mktime)

(define-derived-mode scratch-mode fundamental-mode "scratch" "")

(defun cleanup-scratch-buffers ()
  "kills all empty scratch buffers"
  (interactive)
  (loop for b in (collect-buffers-mode 'scratch-mode) 
	when (= 0 (with-current-buffer  b (buffer-size))) 
	do (kill-buffer b)
	)
  )

(defvar *scratch-pattern* "*scratch*")
(defun make-scratch-name (&optional prefix)
  "generate a non-existing buffer name matching PREFIX"
  (interactive)
	(let ((pat (or prefix *scratch-pattern*))
				bname)
		(loop with i = 0 while (get-buffer (setq bname (format "%s %d" pat i))) do (setq i (1+ i)) finally return bname)
		)
  )

(defun get-scratch-buffer (&optional prefix mode)
	"generate a scratch buffer using optional string PREFIX in name.
default prefix is generated by `mktimestamp'

if optional MODE is specified, enter that mode.

kludgy feature:  if mode is not specified and prefix begins with \".\", then treat it as a postfix
also if postfix is on auto-mode-alist, enter the associated mode.

another kludgy feature: if prefix is a list (as in like a universal-argument), then it is ignored, 
the default mode used is `major-mode'

 (get-scratch-buffer \"el\") will generate a scratch buffer and put into `scratch-mode', 
 (get-scratch-buffer \".el\") will generate a scratch buffer like 1151760141.el and enter `emacs-lisp-mode'
"

  (interactive)

	(let* ((bname (mktimestamp))
				 (mode 
					(cond
					 (mode mode)
					 ((and prefix (listp prefix)) 
						major-mode)
					 ((and prefix (= (aref prefix 0) ?.))
						(setq bname (concat bname prefix))
						(cdr (assoc (concat "\\" prefix  "$") auto-mode-alist)))
					 (t
						'scratch-mode
						)))
				 (b (apply 'zap-buffer
									 (list 
										bname
										mode))))

    (if (interactive-p) (switch-to-buffer b))
    b
    )
  )

(defun pop-to-last-scratch-buffer ()
  "find the most recently used scratch buffer and pop to it
"
  (interactive)
  (let ((b (loop for x being the buffers when (eq (quote scratch-mode) (progn (set-buffer x) major-mode)) return x)))
    (if (buffer-live-p b) (pop-to-buffer b) (message "no scratch buffers found"))))

(defun switch-to-new-scratch-buffer (&optional arg)
  "`get-scratch-buffer' and switch to it
"
  (interactive "P")
  (switch-to-buffer (get-scratch-buffer arg)))

(defun switch-to-new-scratch-buffer-other-window (&optional arg)
  "`get-scratch-buffer' and switch to it in another window
"
  (interactive "P")
  (switch-to-buffer-other-window (get-scratch-buffer arg)))


(defun yank-to-new-scratch-buffer (&optional arg)
  "`switch-to-new-scratch-buffer' and yank into it
"
  (interactive "P")
  (switch-to-buffer (get-scratch-buffer arg))
  (yank)
  )

(defun collect-scratch-buffers ()
  (save-excursion
    (loop for x being the buffers when 
	  (and (buffer-live-p x) 
	       (eq (progn (set-buffer x) major-mode)
		   'scratch-mode)
	       )
	  collect x))
  )

;; (defun roll-scratch-buffers ()
;;   (interactive)
;;   (let ((l (collect-scratch-buffers)))
;;     (roll-buffer-list l)
;;     )
;;   )

;; alternative version that prompts with some of the buffer contents, since the names are unhelpful
(defun roll-scratch-buffers ()
  (interactive)
  (let ((l (collect-scratch-buffers))
	(x (/ (window-width) 2)))
    (roll-list l 
	       (lambda (b)
		  (format "%s\t\t%s..." (buffer-name b) (with-current-buffer  b (tr (buffer-substring (point-min) (min (1- (point-max)) x)) '((?
																		      "\\n"))))))
	       'kill-buffer-1 
	       'switch-to-buffer)
    )
  )
 
(defmacro %% (x y)
  "X modulo Y symmetrical around 0"
  `(let* ((z ,x) (w ,y) (u (if (= 0 w) 0 (% z w)))) (if (< u 0) (+ z w) u))
  )
; (assert (= (%% 0 1) 0))
; (assert (= (%% 1 0) 0))
; (assert (= (%% 0 0) 0))
; (assert (= (%% 4 3) 1))
; (assert (= (%% 4 -3) 1))
; (assert (= (%% -4 3) -1))
; (assert (= (%% -4 -3) -7))

(defun list-scratch-buffers ()
  (interactive)
  (list-buffers-mode 'scratch-mode)
  )

(modify-syntax-entry ?< "(" scratch-mode-syntax-table)
(modify-syntax-entry ?> ")" scratch-mode-syntax-table)

(provide 'scratch-mode)
