(put 'sh 'rcsid
 "$Id$")

(require 'typesafe)
(require 'trim)
(require 'cat-utils)

;; sh -- bain-damaged interpreter for shell scripts
(defvar *assignment-regexp* "[[:blank:]]*\\(export\\)?[[:blank:]]*\\([[:word:]_]+\\)[[:blank:]]*=[[:blank:]]*\\(.*\\)" "regexp matching an assignment statement")

(defvar *comment-regexp* "^[[:blank:]]*#" "regexp matching a shell comment")

(defvar *backquote-regexp* "\`\\(.*\\)\`" "regexp matching a backquote expression")

(defvar *quote-regexp* "\"\\(.*\\)\"" "regexp matching a quoted expression")

(defvar *conditional-regexp* "^[[:blank:]]*\\(if\\)[[:blank:]]+" "regexp matching a conditional statement")
(defvar *end-conditional-regexp*  "[[:blank:]]*\\(fi\\)[[:blank:]]*$" "regexp matching a conditional statement")
(defvar *sh-ignore-conditional* 0)

;; reduced to support just exports, because this is a REALLY BAD IDEA

(defvar *sh-custom-parser* nil "assign to a function that can handle additional shell command lines")

(defvar debug-parse-line nil)
(defvar debug-scan-file nil)

; note: this might get redefined should you ever load doctor.el
(defun $ (value) 
  "Substitute environment variables referred to in FILENAME.
"
  ;; values may be computed expressions
  (when (and (string* value) (string-match *backquote-regexp* value))
    (setq value (eval-shell-command (concat "echo " value)))
    )

  ;; filepaths with spaces often have extra doublequotes around them to disambiguate them in shell scripts; remove those
  (when (and (string* value) (string-match *quote-regexp* value))
    (let ((exp (match-string 1 value)))
      (when (string* exp)
	(setq value exp)
	)
      ))


  (when (string* value)
    (setq value (substitute-in-file-name value))
    )

  value
  )
; (assert (not ($ nil)))
; (assert (progn (setenv "foo" "bar")  (string= ($ "$foo") "bar")))
; (assert (progn (setenv "foo" "baz") (setenv "bar" "bo")  (string= ($ "$foo:$bar") "baz:bo")))
; (assert (string= ($ "`cygpath -u \\`regtool get '/machine/SOFTWARE/Perl/BinDir'\\` | sed -e 's/\\/perl.exe//'`") "/Perl/bin"))

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

(defun sh-parse-line (line)
  "line contains a complete shell command.  turn it into emacs stuff.
"

  (cond
   ((string-match *conditional-regexp* line)
    (unless (string-match *end-conditional-regexp* line) ; one liner
      (setq *sh-ignore-conditional* (1+ *sh-ignore-conditional*))
      ))
   ((string-match *end-conditional-regexp* line)
    (unless (> *sh-ignore-conditional* 0)
      (message "sh warning: *end-conditional-regexp* found, with no matching *conditional-regexp* (%s)" line)
      (when debug-parse-line (debug))
      )
    (setq *sh-ignore-conditional* (1- *sh-ignore-conditional*))
    )
   ((string-match *comment-regexp* line) nil)
   ((string-match *assignment-regexp* line)
    (let ((name (match-string 2 line)) (value (match-string 3 line)))
      (setenv name ($ value))
      (format "%s=%s" name value)))
   ((string-match *backquote-regexp* line)
    (let ((exp (match-string 1 line)))
      (eval-shell-command (format "echo %s" exp))
      )
    )
   ((and *sh-custom-parser* (listp *sh-custom-parser*))
    (loop for parser in *sh-custom-parser* thereis 
	  (apply *sh-custom-parser* (list line))))

   ((and *sh-custom-parser* (functionp *sh-custom-parser*) )
    (apply *sh-custom-parser* (list line)))
   )
  )
; (assert (not (sh-parse-line "# blah")))
; (assert (progn (sh-parse-line "foo=baz") (sh-parse-line "# foo=bar") (string= ($ "$foo") "baz")))
; (assert (progn (sh-parse-line "foo=baz") (string= ($ "$foo") "baz")))
; (assert (progn (sh-parse-line "export foo = bar") (string= ($ "$foo") "bar")))
; (assert (progn (sh-parse-line "foo=") (string= ($ "$foo") "")))
; (assert (progn (sh-parse-line "foo=") (sh-parse-line "if [ $? -eq 0]; then foo=baz; fi") (string= ($ "$foo") "")))
; (assert (progn (mapcar 'sh-parse-line '("foo=" "if [ $? -eq 0]; then" "foo=baz;" "fi")) (string= ($ "$foo") "")))

; for the following example, see: (progn (find-file (expand-file-name ".bashrc" (host-config))) (goto-char (point-min)) (search-forward "PERL_HOME"))
; (progn (sh-parse-line "`cygpath -u \\`regtool get '/machine/SOFTWARE/Perl/BinDir'\\` | sed -e 's/\\/perl.exe//'`")  (getenv "PERL_HOME"))



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
  (setq *sh-ignore-conditional* 0)
  (remove* nil (mapcar 'sh-parse-line (split (read-file fn) "\C-j")))
  (when (> *sh-ignore-conditional* 0)
    (error "sh: *sh-ignore-conditional* still set after parse of file %s" fn)
    (when debug-scan-file (debug))
    )
  (setq *sh-ignore-conditional* 0)
  )
; (scan-file (expand-file-name "~/.bashrc"))
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
