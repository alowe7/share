(put 'typesafe 'rcsid
 "$Id$")

(require 'trim)
(require 'backquote)

; todo: go back and fix all refs

(defmacro string* (s &optional default)
  "evaluates to STRING if non-null and nonzero length, else DEFAULT.
 arguments are evaluated at most once.  default is evaluated only if necessary
"
  `(let ((*s* ,s)) (or (and (stringp *s*) (> (length *s*) 0) *s*) ,default))
  )
; (assert (not (string* "")))
; (assert (eq (string* "" t) t))
; (assert (string= (string* "foo" t) "foo"))
; (assert (string= (string* (progn (read-string "a") "foo") (progn (read-string "b" t))) "foo"))

(defmacro number* (n &optional default)
  "evaluates to NUMBER if a numberp.
  if NUMBER is a string representation of a numberp, then reads the string value.
  otherwise returns optional DEFAULT.
 arguments are evaluated at most once.  default is evaluated only if necessary
"
  `(let ((*n* ,n))
     (cond
      ((numberp *n*) *n*)
      ((and (stringp *n*) 
	    (> (length *n*) 0)
	    (setq *n* (car (read-from-string *n*)))
	    (numberp *n*)) *n*)
      (t ,default)
      )
     )
  )
; (assert (= (number* 1) 1))
; (assert (= (number* "1") 1))
; (assert (= (number* "10.1") 10.1))
; (assert (not (number* "") ))
; (assert (= (number* "" 1) 1))
; (assert (not (number* "abc") ))
; (assert (= (number* "abc" 1) 1))
; (assert (= (number* (progn (read-string "evaluation of first arg: ") "abc") (progn (read-string "evaluation of default arg: ") 1)) 1))
; (assert (= (number* (progn (read-string "evaluation of first arg: ") "100") (progn (read-string "evaluation of default arg: ") 1)) 100))

(defun int* (str)
  "evaluates to integer value of STR if STR represents an integer, nil otherwise"

  (and (stringp str) (string-match "^[0-9]+" str) (eq (match-end 0) (length str)) (string-to-number str))
  )
; (assert (= (int* "1") 1))
; (assert (not (int* "1.1")))
; (assert (not (int* "foo")))

(defmacro read-from-string* (s &optional default)
  "evaluates to intern STRING if non-null and nonzero length, else DEFAULT.
 arguments are evaluated only once"
  `(let ((*s* ,s))
     (if (and (string* *s*) (> (length *s*) 0))
	 (car (read-from-string *s*))
       ,default)
     )
  )
; (assert (eq (read-from-string* "foo" 'bar) 'foo))
; (assert (eq (read-from-string* "" 'bar) 'bar))
; (assert (eq (read-from-string* nil 'bar) 'bar))
; (assert (eq (read-from-string* (progn (read-string "evaluation of first arg: ") "foo") (progn (read-string "evaluation of default arg: ") 'bar)) 'foo))

(defmacro read-string* (prompt &optional default)
  "interactively read for string, prompting with PAT, with default value DEFAULT.
pat may have formatting strings in it, see `format'
the result of `read-string' behaves like `string*' with respect to default behavior
prompt and default are evaluated exactly once.
"
  `(let* ((*prompt* ,prompt)
	  (*default* ,default)
	  (*s* (read-string (format *prompt* *default*))))
     (if (and (stringp *s*) (> (length *s*) 0)) *s* *default*)
     )
  )
; (read-string* "foo (%s): " "bar")

(defmacro read-file-name* (prompt &optional default dir initial)
  "interactively read for filename, prompting with PROMPT, with default value DEFAULT.
prompt may have exactly one formatting string, populated with default value, if present. see `format'.
prompt and default are evaluated exactly once.
additional args DIR and INITIAL are passed to `read-file-name'
"
  `(let* ((*prompt* ,prompt)
	  (*default* ,default)
	  (*s* (read-file-name (format *prompt* *default*) ,dir (expand-file-name *default*) nil ,initial)))
     (if (and (stringp *s*) (> (length *s*) 0)) *s* *default*)
     )
  )
; (read-file-name* "foo(%s): " "bar")

(defmacro read-directory-name* (prompt &optional default dir initial)
  "interactively read for directoryname, prompting with PROMPT, with default value DEFAULT.
prompt may have formatting strings in it, see `format'
prompt and default are evaluated exactly once.
additional args DIR and INITIAL are passed to `read-directory-name'
"
  `(let* ((*prompt* ,prompt)
	  (*dir* ,dir)
	  (*default* (expand-file-name ,default *dir*))
	  (*s* (read-directory-name (format *prompt* *default*) *dir* (expand-file-name *default*) nil ,initial))
	  (*ret*  (expand-file-name *s* ,dir))
	  )
     (if (and (stringp *ret*) (> (length *ret*) 0)) *ret* *default*)
     )
  )
; (read-directory-name* "foo(%s): " "bar" "/tmp")
; (read-directory-name* "foo(%s): " "bar")




(defmacro completing-read* (prompt table &optional default rest)
  "interactively read for string, prompting with PROMPT, completing from LIST with default value DEFAULT.
optional ARGS may be a list of args for `completing-read'
pat may have formatting strings in it, see `format'
the result of `read-string' is passed to `string*'
"
  `(let* ((*prompt* ,prompt)
	  (*table* ,table)
	  (*tableval* (cond ((symbolp *table*) (eval *table*)) ((functionp *table*) (funcall *table*)) (t *table*)))
	  (*d* ,default)
	  (*rest* ,rest)
	  (*s* (apply 'completing-read (nconc (list (format *prompt* *d*) *tableval*) *rest*)))
	  (v (if (or (null *s*) (= (length *s*) 0)) *d* *s*))
	  )
     v
     )
  )
; (completing-read* "foo (%s): " '("a" "b" "c") "c" )  
; (completing-read* "foo (%s): " '("a" "b" "c") "c" '(nil t))  ; same as above but require match
; (completing-read* "thing (%s): "  '(("a" "1") ("b" "2")("c" "3")) "b" '(nil t))
; (let ((table '(("a" "1") ("b" "2")("c" "3")))) (completing-read* "thing (%s): "  table "b" '(nil t)))
; (completing-read* "foo (%s): " (lambda () '("a" "b" "c")) "c" '(nil t))

(defmacro  describe-expression (expr) "EXPR is any lisp expression.  evaluate it and describe the result" (let ((expression (eval expr))) (describe-variable (quote expression))))
; (describe-expression (grab-load-history))

(provide 'typesafe)
