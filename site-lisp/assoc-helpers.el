(put 'assoc-helpers 'rcsid
 "$Id$")

(require 'cl)

(defun modify-alist (x y z)
  (set x (loop for w in (eval x) collect 
	       (if (eq (car w) y) (cons y z) w))
       )
  )

(defmacro add-association (mapping list &optional clobber)
  "add an assoc MAPPING to a-list LIST.
replace an existing mapping only if optional CLOBBER is set
"
  `(let ((m ,mapping)
	 (l (eval ,list)))

     (cond ((not (assoc (car m) l))
	    (push m l))
	   (,clobber
	    (setq l (remove* m l :test (lambda (x y) (equal (car x) (car y)))))
	    (push m l))
	   )

     (set ,list l)
     )
  )
; 
; (assert (let ((test-alist '((a . 1) (b . 2) (c . 3)))) (add-association '(c . 4) 'test-alist )  (= 3 (cdr (assoc 'c test-alist)))))
; (assert (let ((test-alist '((a . 1) (b . 2) (c . 3)))) (add-association '(c . 4) 'test-alist t) (= 4 (cdr (assoc 'c test-alist)))))
; (assert (let ((test-alist '((a . 1) (b . 2) (c . 3)))) (add-association '(d . 4) 'test-alist t) (and (= 3 (cdr (assoc 'c test-alist))) (= 4 (cdr (assoc 'd test-alist))))))

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
