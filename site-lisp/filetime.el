(put 'filetime 'rcsid 
 "$Id$")

(require 'eval-process)

(defun filemodtime (f)
  "returns last modified time of FILE
see `file-attributes'
"
  (and f (elt (file-attributes f) 5))
  )
; (filemodtime "filetime.el")

(defun fileacctime (f)
  "returns last accessed time of FILE
see `file-attributes'
"
  (and f (elt (file-attributes f) 4))
  )
; (fileacctime "filetime.el")

(defun compare-filetime (a b)
  "compare file times A and B.
 returns -1 if A preceeds B, 0 if they're equal, 1 otherwise 

kludgy feature: A and/or B may be strings, if so they are assumed to be filenames, if so use `filemodtime' of file
"

  (let ((a (if (and (stringp a) (file-exists-p a)) (filemodtime a) a))
	(b (if (and (stringp b) (file-exists-p b)) (filemodtime b) b)))
    (cond ((null (or a b)) 0)
	  ((null a) -1)
	  ((null b) 1)
	  ((< (car a) (car b)) -1)
	  ((> (car a) (car b)) 1)
	  ((< (cadr a) (cadr b)) -1)
	  ((> (cadr a) (cadr b)) 1)
	  (t 0))
    )
  )
;  (assert (= 0 (compare-filetime "filetime.el" "filetime.el")))
;  (assert (= 0 (compare-filetime "filetime.el" (filemodtime "filetime.el"))))
;  (assert (= 0 (compare-filetime (filemodtime "filetime.el") "filetime.el")))
;  (assert (= 0 (compare-filetime (filemodtime "filetime.el") (filemodtime "filetime.el"))))
; (assert (not (= 0 (compare-filetime "filetime.el" "/var/log/messages"))))

(defun ftime () (interactive)
  "display formatted time string last modification time of file for current buffer"
  (let ((f (filemodtime (buffer-file-name))))
    (message (if f
		 (clean-string (eval-process "mktime" (format "%d" (car f)) (format "%d" (cadr f))))
	       "no file")
	     )
    )
  )
; (ftime)

(defun delta-time (t1 t2)
  " return absolute difference between T1 and T2  see `current-time' for format
normally assert t1 > t2
if it is not, then the arguments are reversed in order to return the absolute value of the delta
"

  (let (t3) 
  ; make sure t1 > t2
    (if (< (compare-filetime t1 t2) 0)
	(setq t3 t1
	      t1 t2
	      t2 t3))

    (let ((u (- (car t1) (car t2)))
	  (l (- (cadr t1) (cadr t2)))
  ; allow either microsecond count to be nil, meaning 0
	  (m (- (or (caddr t1) 0) (or (caddr t2) 0))))
      (if (< m 0) (setq l (1- l) 
			m (- 1000000 m)))
      (if (< l 0) (setq u (1- u)
			l (- 65535 l)))
      (list u l m)
      )
    )
  )

(provide 'filetime)
