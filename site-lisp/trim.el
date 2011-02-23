(put 'trim 'rcsid
 "$Id$")

(require 'long-comment)

(defvar *leading-whitespace-pattern* "^[ \t]+")
(defvar *trailing-whitespace-pattern* "[ \t\r]+$")
(defvar *blank-line-pattern* "[\r\n][\r\n]+")

(defun trim-trailing-white-space-string (&optional s)
  (interactive "strim trailing white space from string: ")
  (and s 
       (let* ((p (string-match *trailing-whitespace-pattern* s)))
	 (if p (substring s 0 p) s))
       )
  )
; (assert (string= (trim-trailing-white-space-string "foo 	") "foo"))
; (assert (string= (trim-trailing-white-space-string "foo") "foo"))

(defun trim-trailing-white-space-region (&optional beg end)
  " trim trailing white space from region
when called from a program, takes two args BEG and END 
"
  (interactive "r")
; work backwards as end may move
  (save-excursion
    (goto-char end)
    (while (re-search-backward *trailing-whitespace-pattern* beg t)
      (replace-match ""))
    )
  )
(/*
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo 	")
	    (trim-trailing-white-space-region (point-min) (point-max))
	    (buffer-string)
	    )
	  "bar baz bo"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo 	\n")
	    (let ((p (point)))
	      (insert "binga banga bunga	 \n")
	      (trim-trailing-white-space-region p (point-max))
	      (buffer-string)
	      )
	    )
	  "bar baz bo 	\nbinga banga bunga\n"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo 	\n")
	    (let ((p (point)))
	      (insert "f\t\n")
	      (insert "binga banga bunga	 \n")
	      (trim-trailing-white-space-region (point-min) p)
	      (buffer-string)
	      )
	    )
	  "bar baz bo\nf\t\nbinga banga bunga	 \n"
	  )
	 )
 */)

(defun trim-trailing-white-space ()
  "trim trailing white space from `point' to `point-max'
"
  (interactive)
  (trim-trailing-white-space-region (point) (point-max))
  )

(defun trim-leading-white-space-string (&optional s)
  (interactive "strim leading white space from string: ")
  (and s 
       (let* ((p (string-match *leading-whitespace-pattern* s)))
	 (if p (substring s (match-end 0)) s))
       )
  )
; (assert (string= (trim-leading-white-space-string " 	foo") "foo"))
; (assert (string= (trim-leading-white-space-string "foo") "foo"))

(defun trim-leading-white-space-region (&optional beg end)
  " trim leading white space from region
when called from a program, takes two args BEG and END 
"
  (interactive "r")
; work backwards as end will move
  (save-excursion
    (goto-char end)
    (while (re-search-backward *leading-whitespace-pattern* beg t)
      (replace-match ""))
    )
  )
(/*
 (assert (string=
	  (with-temp-buffer
	    (insert " 	bar baz bo 	")
	    (trim-leading-white-space-region (point-min) (point-max))
	    (buffer-string)
	    )
	  "bar baz bo 	"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert " \tbar baz bo\t \n")
	    (let ((p (point)))
	      (insert " \tbinga banga bunga\t \n")
	      (trim-leading-white-space-region p (point-max))
	      (buffer-string)
	      )
	    )
	  " \tbar baz bo\t \nbinga banga bunga\t \n"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert " \tbar baz bo\t \n")
	    (let ((p (point)))
	      (insert " \tbinga banga bunga\t \n")
	      (trim-leading-white-space-region (point-min) p)
	      (buffer-string)
	      )
	    )
	  "bar baz bo\t \n \tbinga banga bunga\t \n"
	  )
	 )
 */)

(defun trim-leading-white-space ()
  "trim leading white space from `point' to `point-max'
"
  (interactive)
  (trim-leading-white-space-region (point) (point-max))
  )

(defun trim-white-space-string (&optional s) 
  "trim white-space in STRING
"
  (interactive "strim white space from string: ")
  (trim-trailing-white-space-string (trim-leading-white-space-string s))
  )
; (assert (string= (trim-white-space-string " \tfoo\t ") "foo"))

(defun trim-white-space-region (beg end)
  "trim white space from region
when called from a program, requires two args: BEG and END
"
  (interactive "r")
  (trim-leading-white-space-region beg end)
  (trim-trailing-white-space-region beg end)
  )

(defun trim-white-space () 
  "trim white space from `point' to `point-max'
"
  (interactive)
  (trim-white-space-region (point) (point-max))
  )

(defun trim-blank-lines-string (s)
  "trim blank lines from STRING
"
  (interactive "strim blank lines from string: ")
  (and s 
       (replace-regexp-in-string *blank-line-pattern* "\n" s)
       )
  )
; (assert (string= (trim-blank-lines-string "foo\n\nbar")  "foo\nbar"))

(defun trim-blank-lines-region (beg end)
  "trim blank lines from region.
"
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward *blank-line-pattern* beg t)
      (replace-match "\n" nil nil))
    )
  )
(/*
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo\n\n")
	    (trim-blank-lines-region (point-min) (point-max))
	    (buffer-string)
	    )
	  "bar baz bo\n"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo\n\n")
	    (let ((p (point)))
	      (insert "binga banga bunga\n\n")
	      (trim-blank-lines-region p (point-max))
	      (buffer-string)
	      )
	    )
	  "bar baz bo\n\nbinga banga bunga\n"
	  )
	 )
 (assert (string=
	  (with-temp-buffer
	    (insert "bar baz bo\n\n\n")
	    (let ((p (point)))
	      (insert "f\n\n")
	      (insert "binga banga bunga\n\n")
	      (trim-blank-lines-region (point-min) p)
	      (buffer-string)
	      )
	    )
	  "bar baz bo\nf\n\nbinga banga bunga\n\n"
	  )
	 )
 */)

(defun trim-blank-lines ()
  "trim blank lines from `point' to `point-max'
"
  (interactive)

  (trim-blank-lines-region (point) (point-max))
  )

(fset 'trim 'trim-white-space-string)

(provide 'trim)
