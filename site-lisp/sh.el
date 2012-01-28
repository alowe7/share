(put 'sh 'rcsid
 "$Id$")

(require 'typesafe)
(require 'trim)
(require 'cat-utils)

;; sh -- bain-damaged interpreter for shell scripts

; note: this might get redefined should you ever load doctor.el
(defun $ (filename) 
  "Substitute environment variables referred to in FILENAME.
"

  (and  
   (string* filename)
   (substitute-in-file-name filename)
   )
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

(defun -d (x)  "THING is a dir" (and (string* x) (file-directory-p ($ x)) x))
(defun -z (x) "THING is null or string of zero length" (or (null x) (= 0 (length ($ x)))))
(defun -n (x) "THING is neither null nor a string of zero length" (not (-z x)))

(defvar *assignment-regexp* "[[:blank:]]*\\(export\\)?[[:blank:]]*\\([[:word:]_]+\\)[[:blank:]]*=[[:blank:]]*\\(.+\\)" "regexp matching an assignment statement")

(defvar *comment-regexp* "^[[:blank:]]*#" "regexp matching a shell comment")

;; reduced to support just exports, because this is a REALLY BAD IDEA

(defvar *sh-custom-parser* nil "assign to a function that can handle additional shell command lines")

(defun sh-parse-line (line)
  "line contains a complete shell command.  turn it into emacs stuff.
"

  (cond
   ((string-match *comment-regexp* line) nil)
   ((string-match *assignment-regexp* line)
    (let ((name (match-string 2 line)) (value (match-string 3 line)))
      (setenv name ($ value))
      (format "%s=%s" name value)))
   ((and *sh-custom-parser* (listp *sh-custom-parser*))
    (loop for parser in *sh-custom-parser* thereis 
	  (apply *sh-custom-parser* (list line))))

   ((functionp *sh-custom-parser*) 
    (apply *sh-custom-parser* (list line)))
   )
  )
; (assert (not (sh-parse-line "# blah")))
; (assert (progn (sh-parse-line "foo=baz") (sh-parse-line "# foo=bar") (string= ($ "$foo") "baz")))
; (assert (progn (sh-parse-line "foo=baz") (string= ($ "$foo") "baz")))
; (assert (progn (sh-parse-line "export foo = bar") (string= ($ "$foo") "bar")))
; (assert (progn (sh-parse-line "foo=") (string= ($ "$foo") "bar")))

(defun sh-parse-indicated-line ()
  (interactive)
  (let* ((line (thing-at-point 'line)))
    (sh-parse-line line)
    )
  )


(defun scan-file (fn)
  "interpret shell script FILE to some extent.
return list of command evaluation values with nil removed
"
  (interactive "ffilename: ")
  (remove* nil (mapcar 'sh-parse-line (split (read-file fn) "\C-j")))
  )
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
