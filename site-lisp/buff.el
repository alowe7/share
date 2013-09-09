;; $Id$

(require 'typesafe)
(require 'fapropos)



; (display-buffer (list-buffers-noselect nil (collect-buffers-mode 'dired-mode)))
; (display-buffer (list-buffers-noselect nil (collect-buffers-named "buf")))
; (display-buffer (list-buffers-noselect nil (collect-buffers-mode 'shell-mode)))


(defun list-buffers-helper (pred bn)
    (when (buffer-live-p (get-buffer bn))
      (condition-case x (kill-buffer bn) (error nil)))

    (let ((b (list-buffers-noselect nil (funcall pred))))
      (with-current-buffer b (rename-buffer bn))
      (display-buffer b)
      )
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
			    (mapcar (lambda (x) 
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
