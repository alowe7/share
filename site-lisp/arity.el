

(defun cardinality (body)
  " like `subr-arity', but works for symbol-functions, lambdas and byte-compiled functions as well as subrs
"
  (length (arglist body))
  )
; (assert (= 2 (cardinality 'setcdr)))
; (assert (= 0 (cardinality 'html-mode)))
; (assert (= 1 (cardinality '(lambda (foo))))

(defun arglist (body)
  "wow. all this just to get the arglist...
returns only required args
"
  (cond
   ((and (listp body) (eq (car body) 'lambda))
    (cadr body))
   ((and (symbolp body) (fboundp body))
  ; its a function
    (let (
	  (sym body)
	  (body (symbol-function body))
	  )
      (cond ((symbolp body)
  ; if func is another symbol, recurse
	     (arglist body))
	    (t
	     (let* ((args
		     (copy-list 
		      (cond
		       ((subrp body)
  ; whew boy
			(ad-subr-arglist sym)
			)
  ; if func is byte compiled, its an array
		       ((byte-code-function-p body)
			(aref body 0))
  ; else its a lambda
		       (t (cadr body)))
		      ))
  ; ignore optional args
		    (pos (and args (or (position '&optional args) (position '&rest args)))))
	       (cond
		((or (null pos) (< pos 1)) args)
		(t  (progn (setcdr (nthcdr (1- pos) args) nil) args))
		)
	       )
	     )
	    )
      )
    )
   )
  )

; (arglist 'rplacd)
; (arglist 'eval-p)
; (arglist 'zap-buffer)
; (arglist 'message)
; (let ((body 'message)) (length (arglist body)))

(defun eval-p (body)
  " eval BODY.  
kludgy special case if  body is a symbol with a function definition of no (required) args, eval the function def"
  (cond 
   ((and (functionp body) (= (length (arglist body)) 0))
  ; its a function of no args.  call it.
    (funcall body)
    )
   (t (eval body))
   )
  )

; (condition-case x (eval-p 'message) (error (cond ((eq (car x) 'wrong-number-of-arguments) (message "expected result: wrong-number-of-arguments")) (t (apply 'error x)))))

(provide 'arity)
