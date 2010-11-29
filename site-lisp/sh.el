(put 'sh 'rcsid
 "$Id$")


;; sh -- bain-damaged interpreter for shell scripts

; note: this might get redefined should you ever load doctor.el
(defun $ (filename) 
  "Substitute environment variables referred to in FILENAME.
"

  (expand-file-name (substitute-in-file-name filename))
  )
; ($ "$HOME:$USERNAME")

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

(provide 'sh)
