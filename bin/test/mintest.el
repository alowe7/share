(put 'post-advise 'rcsid
 "$Id$")

(require 'ctl-ret)


(defun check-if-advised ()
  (interactive)
  (let* ((thing (thing-at-point 'symbol))
	 (sym (intern thing))
	 advice)
    (cond
     ((not (fboundp sym))
      (message "%s is not fbound" thing))
     ((not (setq advice (ad-is-advised sym)))
      (message "%s is not advised" thing))
     (t 
      (save-excursion
	(with-output-to-temp-buffer (help-buffer)
	  (set-buffer standard-output)
	  (insert thing)
	  ;; Use " is " instead of a colon so that
	  ;; it is easier to get out the function name using forward-sexp.
	  (princ "\t has advice \n")
	  (pp (list advice))
	  (with-current-buffer standard-output
	    ;; Return the text we displayed.
	    (buffer-string))))
      )
     )
    )
  )
(define-key ctl-RET-map "\C-i" 'check-if-advised)

(defun toggle-advise ()
)
(define-key ctl-RET-map "\C-u" 'toggle-advise)

