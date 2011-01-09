(put 'cat-seq 'rcsid
 "$Id: cat-seq.el 890 2010-10-04 03:34:24Z svn $")

(require 'cat-utils)

;; utilities for converting between strings and lists or vectors of strings

;;; todo generalize this by adding fn to apply
(defun catvectorint (s &optional c)
  "list is a string of integers separated by character c (default ':')
  return its value as a vector of ints"
  (interactive "spath: ")
  (apply 'vector (catlistint s c))
  )


(defun catlistint (s &optional c)
  "list is a string of ints separated by character c (default ':')
  return its value as a list of ints"
  (interactive "spath: ")
  (mapcar 'string-to-int (catlist s c))
  )

(defun catvector (s &optional c)
  "list is a string of words separated by character c (default ':')
  return its value as a list of strings"
  (interactive "spath: ")
  (apply 'vector (mapcar 'intern (catlist s c)))
  )

(defun catlist (s &optional c)
  "string is a string of words separated by character c (default ':')
  return its value as a list of strings"
  (interactive "spath: ")
  (let* ((cc (or c ?:))
	(pat (format "[^%c]*%c" cc cc))
	(l nil)
	(result nil))
    (while s
      (let ((w (and (string-match pat s)
		    (substring s (match-beginning 0)
			       (1- (match-end 0))))))
	(setq l (append l (list (or w s))))
	(if w 
	    (setq s (substring s (match-end 0)))
	  (setq s nil))
	))
    (dolist (x l) (and (> (length x) 0) (setq result (nconc result (list x)))))
    result
  )
)

(defun catfile (f &optional c)
  "file is a file containing a list of words separated by character c (default '\n')
  return its value as a list of strings"
  (interactive "spath: ")
  (catlist (read-file f) (or c ?
														 ))
)

(defun catalist (s &optional c)
  "list is a string of words separated by character c (default ':')
  return its value as an alist of strings"
  (interactive "spath: ")
  (let* ((cc (or c ?:))
	(pat (format "[^%c]*%c" cc cc))
	(l nil)
	(result nil))
    (while s
      (let ((w (and (string-match pat s)
		    (substring s (match-beginning 0)
			       (1- (match-end 0))))))
	(setq l (append l (list (or w s))))
	(if w 
	    (setq s (substring s (match-end 0)))
	  (setq s nil))
	))
    (dolist (x l) (and (> (length x) 0) (setq result (nconc result (list (list x))))))
    result
  )
)

(defun catpath (path &optional c)
  "path is an environment variable representing a path.  
  return its value as a list of strings
  elements of path are separated by character c (default ':')
"
  (interactive "spath: ")

  (let ((path (getenv path)))
    (and path
	 (let ((c (if (or (eq c 'w32)
			  (string-match ":\\\\"  path))
		      semicolon
		    (or c ":"))))
		 
	   (loop for x in (split path c)
		 collect (expand-file-name x))
	   )
	 )
    )
  )

(defun uncatlist (list &optional s)
  " list is a list of strings.  concat them separated by optional
string s (default \":\")"

  (let (result (ps (or s ":")))
    (loop for x in list do
	  (setq result (concat result x ps)))
    (substring result 0 (- (length ps)))))

(provide 'cat-seq)
