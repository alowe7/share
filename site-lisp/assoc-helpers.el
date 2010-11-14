(put 'assoc-helpers 'rcsid
 "$Id: assoc-helpers.el 890 2010-10-04 03:34:24Z svn $")

(require 'cl)

(defun modify-alist (x y z)
  (set x (loop for w in (eval x) collect 
	       (if (eq (car w) y) (cons y z) w))
       )
  )

(defun add-association (mapping list &optional clobber)
  "add an assoc MAPPING to a-list LIST.
clobber an existing mapping if optional CLOBBER is nonnil
"
  (let ((l (eval list)))
    (cond ((not (assoc (car mapping) l))
	   (push mapping l))
	  (clobber
	   (setq l (remove* mapping l :test '(lambda (x y)     
					       (equal (car x) (car y)))))
	   (push mapping l))
	  )
    (set list l)
    )
  )
; (setq x '((a 1) (b 2) (c 3)))
; (add-association '(d 4) 'x t)

(defun remove-association (key list)
  "remove any assoc mapping key in a-list LIST.
uses `string='
"
  (if (symbolp list)
      (set list (remove* key (eval list) :test (lambda (x y) (string= x (car y)))))
    (remove* key (eval list) :test (lambda (x y) (string= x (car y)))))
  )

; (let ((l '(("bar" 1) ("foo" 2) ("foo" 3))))  (remove-association "foo" 'l)  l)
;  (remove-association "foo" '(("bar" 1) ("foo" 2) ("foo" 3)))


(defun assocd (a l d)
  " like assoc, but return d if a is not on l"
  (let ((v (cdr (assoc a l))))
    (or v d)))


(provide 'assoc-helpers)
