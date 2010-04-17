(put 'typesafe 'rcsid
 "$Id: typesafe.el,v 1.1 2010-04-17 17:55:03 keystone Exp $")

(require 'trim)

; todo: go back and fix all refs
(defmacro string* (**s** &optional **default**)
  "evaluates to STRING if non-null and nonzero length, else DEFAULT.
 arguments are evaluated only once"
  (let ((*s* (eval **s**))) (or (and (sequencep *s*) (> (length *s*) 0) *s*) (eval **default**))))

(defmacro number* (**n** &optional **default**)
  "evaluates to NUMBER if a numberp.
  if NUMBER is a string representation of a numberp, then reads the string value.
  otherwise returns optional DEFAULT.
 arguments are evaluated only once"
  (let ((*n* (eval **n**)))
    (cond
     ((numberp *n*) *n*)
     ((sequencep *n*)
      (let ((*n* (trim *n*)))
	(cond ((> (length *n*) 0)
	       (car (read-from-string (trim *n*))))
	      (t (eval **default**)))))
     (t (eval **default**))
     )
    )
  )

; alternative implementation:
(defun int* (str)
  "evaluates to integer value of STR if STR represents an integer, nil otherwise"

  (and (stringp str) (string-match "^[0-9]+" str) (eq (match-end 0) (length str)) (string-to-number str))
  )

(defmacro read-from-string* (**s** &optional **default**)
  "evaluates to intern STRING if non-null and nonzero length, else DEFAULT.
 arguments are evaluated only once"
  (let ((*s* (eval **s**))) (if (and (sequencep *s*) (> (length *s*) 0)) (car (read-from-string *s*))
				     (eval **default**))))

(defmacro read-string* (**prompt** &optional **default**)
  "interactively read for string, prompting with PAT, with default value DEFAULT.
pat may have formatting strings in it, see `format'
the result of `read-string' is passed to `string*'
"
  (let* ((*prompt* (eval **prompt**))
	 (*d* (eval **default**))
	 (*s* (read-string (format *prompt* *d*))))
 
    (if (and (sequencep *s*) (> (length *s*) 0)) *s* *d*)
    )
  )

(defmacro read-file-name* (**prompt** &optional **default**)
  "interactively read for existing filename, prompting with PROMPT, with default value DEFAULT.
prompt may have formatting strings in it, see `format'

see `read-file-name'
"
  (let* ((*prompt* (eval **prompt**))
	 (*d* (eval **default**))
	 (*s* (read-file-name (format *prompt* *d*) *d*)))
 
    (or (and (sequencep *s*) (> (length *s*) 0) *s*) *d*)
    )
  )

(defmacro completing-read* (**prompt** **table** &optional **default** **rest**)
  "interactively read for string, prompting with PROMPT, completing from LIST with default value DEFAULT.
optional ARGS may be a list of args for `completing-read'
pat may have formatting strings in it, see `format'
the result of `read-string' is passed to `string*'
"
  (let* ((*prompt* (eval **prompt**))
	 (*d* (eval **default**))
	 (*table* (eval **table**))
	 (*rest* (eval **rest**))
	 (*s* (apply 'completing-read (nconc (list (format *prompt* *d*) *table*) *rest*)))
	 (v (if (or (null *s*) (= (length *s*) 0)) *d* *s*))
	 )

    v
    )
  )


(provide 'typesafe)
