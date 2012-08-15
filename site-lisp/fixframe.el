(put 'fixframe 'rcsid
 "$Id$")

(require 'assoc-helpers)

(defun fixed-font ()
  (let ((font (when (and (eq window-system 'x) (fboundp 'x-list-fonts))
		(loop for page in '("iso8859" "iso10646") thereis
		      (loop for size in '(14 12 10) thereis
			    (loop for dpi in '(100 75) thereis
				  (let ((pat (format "-*-*-medium-r-normal--%d-%d-*-*-m-*-%s-*" size dpi page)))
				    (car (x-list-fonts pat)))
				  )
			    )
		      ))))

    (or font
  ; just make one up
  ;    "-*-Courier-*-r-*-*-18-normal-*-*-*-*-*-*-"
  ; "-*-Lucida Console-normal-r-*-*-15-normal-*-*-*-*-*-*-"
	"-*-Consolas-normal-r-*-*-15-normal-*-*-*-*-*-*-"
	)
    )
  )
; (fixed-font)

(defvar *fixed-frame-parameters*
 `((font  . ,(fixed-font)) (top . 100) (left . 100) (width . 100) (height . 30)))

(defun switch-to-buffer-fixed-frame (b &optional parameters)
  (interactive "bswitch to buffer fixed frame: ")
  (let ((default-frame-alist default-frame-alist))

    (loop for x in *fixed-frame-parameters* do
	  (add-association x 'default-frame-alist t))

    (loop for x in parameters do 
	  (add-association x 'default-frame-alist t))

    (if (eq window-system 'x)
	(switch-to-buffer-other-frame b)
  ; use internal method, since switch-to-buffer-other-frame is advised to clone frames
      (let ((pop-up-frames t))
	(pop-to-buffer b t)
	(raise-frame (window-frame (selected-window))))
      )
    )
  )

(defun find-file-fixed-frame (filename &optional wildcards)
  (interactive "FFind file fixed frame: \np")
; todo be smarter in dired-mode
  (let ((b  (find-file-noselect filename)))
    (if (listp b)
	(mapcar 'switch-to-buffer-fixed-frame (nreverse b))
      (switch-to-buffer-fixed-frame b))
    )
  )

; (call-interactively 'switch-to-buffer-fixed-frame)
(define-key ctl-x-5-map "x" 'switch-to-buffer-fixed-frame)
(define-key ctl-x-4-map "x" 'find-file-fixed-frame)

(provide 'fixframe)
