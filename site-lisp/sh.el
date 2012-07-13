(put 'sh 'rcsid
 "$Id$")

(require 'typesafe)
(require 'trim)
(require 'cat-utils)

;; sh -- bain-damaged interpreter for shell scripts

; note: this might get redefined should you ever load doctor.el
(defun $ (value) 
  "Substitute environment variables referred to in FILENAME.
"

  (when (string* value)
    (setq value (substitute-in-file-name value))
    )

  value
  )
; (assert (not ($ nil)))
; (assert (progn (setenv "foo" "bar")  (string= ($ "$foo") "bar")))
; (assert (progn (setenv "foo" "baz") (setenv "bar" "bo")  (string= ($ "$foo:$bar") "baz:bo")))

(defun ! (x) (not x))

(defun -a (x) 
  "THING exists and is a file"
  (and (file-exists-p ($ x)) x))

(defun -f (x)
  "THING is a file and not a directory
if thing has environment variables embedded, they are expanded
"
  (let ((y  ($ x))) (and (file-exists-p y) (not (file-directory-p y)) x))
  )

(defun -x (x) 
  "THING exists, is a non-directory file and is executable
if thing has environment variables embedded, they are expanded
"
  (let ((y  ($ x)) attrs)
    (and (file-exists-p y)
	 (not (file-directory-p y))
	 (eq (elt (nth 8 (file-attributes y)) 3) ?x)))
  )


(defun -r (x) 
  "THING is a file that is readable
thing is `$' expanded
"
  ;; todo check file-modes
  (and (file-exists-p ($ x)) x))

(defun -d (x) "THING is a dir" (and (string* x) (file-directory-p ($ x)) x))
(defun -z (x) "THING is null or string of zero length" (or (null x) (= 0 (length ($ x)))))
(defun -n (x) "THING is neither null nor a string of zero length" (not (-z x)))

(defun scan-file (fn &optional list-only)
  "interpret shell script FILE; propagate changes back into process environment.
returns FILE
with optional second arg LIST-ONLY, return a list of changed environment variables, but don't set them
"
  (let* ((environment-vars (loop for x in (split (eval-shell-command (format "bash -c '(. %s; env)'" fn)) "\C-j") collect (split x "=")))
	 (changed-environment-vars (loop for x in environment-vars
					 when 
					 (let ((v (getenv (car x)))) 
					   (or (not v)
					       (not (string= (cadr x) v))))
					 collect x)))
    
    (if list-only 
	changed-environment-vars
      (loop for x in changed-environment-vars
	    do
	    (setenv (car x) (cadr x))
	    )
      fn
      )
    )
  )
; (let ((l (scan-file (expand-file-name "~/.bashrc")))) (describe-variable 'l))
; (scan-file (expand-file-name "~/.private/.xdbrc"))

(defun scan-file-p (fn)
  "scan FILE as a shell script.
return nil if file does not exist or cannot be read.  else returns the filename
"

  (interactive (list 
		(read-file-name* (format "file to scan (%s): " (thing-at-point 'filename)) (thing-at-point 'filename))))
		   
  (if (-r fn) (scan-file fn))

  )

; (scan-file-p (expand-file-name "~/.private/.zdbrc"))

(provide 'sh)
