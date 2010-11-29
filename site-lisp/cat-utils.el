(put 'cat-utils 'rcsid
 "$id: cat-utils.el,v 1.4 2004/03/11 19:01:01 cvs exp $")

(require 'backquote)
(require 'typesafe)
(require 'cl)
(require 'thingatpt)

;; perl like apis 

(defun join (seq &optional sep printfn)
  "concatenate elements of sequence sepearted by optional sep.
with third optional arg, apply printfn to elements of seq to obtain strings.
seq may be a list or vector
sep may be a char or a string
"

  (let ((l 
	 (cond ((listp seq) seq)
	       ((vectorp seq) (loop for x across seq collect x))
	       (t nil)))
	(sep (cond ((characterp sep) (char-to-string sep)) (t sep)))
	)
    (mapconcat 'identity l sep)
    )
  )
; (join '("foo" "bar") " ")
; (join (list "a" "b" "c") ":")
; (join (list "a" "b" "c") ?:)
; (join (vector "a" "b" "c") "-")



(defun split (s &optional pat keep-empty-strings)
  "split STRING into a list at regexp or character PAT
if PAT is not specified, splits on all white space: [SPC, TAB, RET]
removes empty strings unless optional third parameter KEEP-EMPTY-STRINGS is set
"
  (and s
       (let* ((start 0)
	      (pat (cond 
		    ((characterp pat) (format "%c" pat))
		    ((string* pat) pat) 
		    (t "[ \C-i\C-j]")))
	      (ret
	       (nconc 
		(loop
		 while (string-match pat s start)
		 collect (prog1 (substring s start (match-beginning 0)) (setq start (match-end 0)))
		 )
		(list (substring s start))
		)))

	 (if keep-empty-strings
	     ret
	   (remove* "" ret :test 'string=)
	   )

	 )
       )
  )
; (split "abcd efgh, ijkl	mnop  " )
; (split "foo;bar;baz" ?;)
; (split "-outline-Arial-bold-r-normal-normal-13-97-96-96-p-60-iso10646-1" "-" t)

(defun split2 (s &optional pat)
  "more scalable version of `split'
except if PAT is not specifed, splits on newline, rather than all whitespace
"
  (let ((pat (or pat "\n"))
	(pos 0) pos2 res)
    (while (setq pos2 (string-match pat s pos))
      (push (substring s pos pos2) res)
      (setq pos (1+ pos2)))
    res
    )
  )

(defun splice (l1 l2)
  "join two lists L1 and L2 into an a-list consisting of the cars in each"
  (let* ((v1 (apply 'vector l1))
	 (v2 (apply 'vector l2))
	 (n (1- (min (length v1) (length v2)))))
    (loop for i from 0 to n collect (cons (aref v1 i) (aref v2 i)))
    )
  )
; (splice '(a b c) '(1 2 3))

;; perl push and pop operate on the tail of the list
;; perl shift and unshift operate on the head

;; since emacs push and pop operate on the head of the list
;; we define shift and unshift to operate on the tail

; todo: try to eval place only once
(defmacro shift (place)
  "remove and return the tail of the list stored in PLACE
"
  (if (and 
       (symbolp place)
       (eval `(listp ,place))
       (eval `(= (length ,place) 1)))
      `(prog1 (car ,place) (setq ,place nil))
    `(prog1 (car (last ,place)) (nbutlast ,place 1))
    )
  )
; (let ((l '(a b c))) (list (shift l) l))
; (shift '(a b c))

; note unshift uses perl arglist, backwards from elisp push
(defmacro unshift (place thing)
  "does the opposite of shift, or push, depending on how you look at it
adds to tail of PLACE THING
"
  (if (symbolp place)
      `(setq ,place (nconc ,place (list ,thing)))
    `(nconc ,place (list ,thing)))
  )
; (let ((l '(a b c))) (unshift l 'd) l)
; (unshift '(a b c) 'd)

(defun shift-word (s)
  "break string into (first-word . rest)"
  (if (string-match "\\W" s)
      (list (substring s 0 (match-beginning 0)) (substring s (match-end 0))))
  )

; (shift-word "foo;bar baz bo")

(defun chop (s)
  " chop trailing char"
  (substring s 0 -1)
  )

(defun chomp (s &optional c)
  "maybe chop trailing linefeed"
  (cond ((not (string* s)) s)
	((eq (aref (substring s -1) 0) (or c ?
					   )	  )
	 (substring s 0 -1))
	(t s))
  )

(defun basename (f)
  "perl flavored composition of `file-name-sans-extension' and `file-name-nondirectory'
"
  (file-name-sans-extension (file-name-nondirectory f))
  )

(defun cmp (s1 s2 &optional ignore-case) 
  "perl flavored version of `compare-strings'
"
  (compare-strings s1 nil nil s2 nil nil ignore-case) 
  )

(defun bgets () 
  (chomp (thing-at-point (quote line)))
  )

(provide 'cat-utils)
