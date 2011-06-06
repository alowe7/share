(put 'cat-utils 'rcsid
 "$id: cat-utils.el,v 1.4 2004/03/11 19:01:01 cvs exp $")

(require 'backquote)
(require 'typesafe)
(require 'cl)
(require 'thingatpt)

;; perl like apis 

(defun join (seq &optional sep printfn)
  "concatenate elements of SEQUENCE sepearted by optional SEP.
sequence may be a list or vector.
sep may be a char or a string.  default is no separator
with optional third arg, apply PRINTFN to elements of sequence.
printfn should accept a single argument and return string values .
"

  (let ((l (cond ((listp seq) seq)
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

; if s begins with a quote then only match a terminating quote
(defmacro xmatch (pat string start) `(let* ((p ,pat) (s ,string) (r ,start)) (and (< r (length s)) (let* ((c (aref s r)) (p2 (cond ((eq ?\" c) (concat "\"" p)) ((eq ?\' c) (concat "\'" p)) (t p))) (v (string-match p2 s r)))  (cond ((null v) v) (t (+ v (- (length p2) (length p)))))))))
; (assert (string= "\"bar\"" (let* ((teststring "foo,\"bar\",baz") (start 4) (v (xmatch "," teststring start))) (if v (substring teststring start v) (substring teststring start)))))
; (assert (string= "\"bar,baz\"" (let* ((teststring "foo,\"bar,baz\"") (start 4) (v (xmatch "," teststring start))) (if v (substring teststring start v) (substring teststring start)))))
; (assert (string= "\"bar,baz\"" (let* ((teststring "foo,\"bar,baz\",bo") (start 4) (v (xmatch "," teststring start))) (if v (substring teststring start v) (substring teststring start)))))
; (assert (string= "\"foo,bar\"" (let* ((teststring "\"foo,bar\",baz") (start 0) (v (xmatch "," teststring start))) (if v (substring teststring start v) (substring teststring start)))))
; (assert (string= "foo" (let* ((teststring "foo,bar,baz") (start 0) (v (xmatch "," teststring start))) (if v (substring teststring start v) (substring teststring start)))))
; (assert (= 4 (xmatch  "[ \C-i\C-j]"  "abcd efgh, ijkl	mnop  "  0)))
; (assert (null (xmatch  "[ \C-i\C-j]"  "abcd efgh, ijkl	mnop  "  22)))

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
		 with end = start
  ; return from xmatch not always = (match-beginning 0)
		 while (setq end (xmatch pat s start))
		 collect (prog1 
			     (substring s start end) 
			   (setq start (match-end 0))
			   )
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

; (assert (equal (split "abcd efgh, ijkl	mnop  " ) '("abcd" "efgh," "ijkl" "mnop")))
;  (assert (equal (split "foo;bar;baz" ?;) '("foo" "bar" "baz")))
;  (assert (equal  (split "-outline-Arial-bold-r-normal-normal-13-97-96-96-p-60-iso10646-1" "-" t) '("" "outline" "Arial" "bold" "r" "normal" "normal" "13" "97" "96" "96" "p" "60" "iso10646" "1")))
;  (assert (equal (split "foo,\"Tuesday, Jan 25 17:25:26 CST 2011\",bar" ",") '("foo" "\"Tuesday, Jan 25 17:25:26 CST 2011\"" "bar")))
;  (assert (equal (split "foo,bar,\"Tuesday, Jan 25 17:25:26 CST 2011\"" ",") '("foo" "bar" "\"Tuesday, Jan 25 17:25:26 CST 2011\"")))

(defun splitf (s &optional pat)
  "return the list of nonblank elements resulting from splitting STRING on regular expression PAT.
basically more scalable version of `split' except if PAT is not specifed, splits on newline, rather than all whitespace
"
  (let ((pat (or pat "\n"))
	(pos 0) pos2 res)
    (while (setq pos2 (string-match pat s pos))
      (setq res (append res  (list (substring s pos pos2))))
      (setq pos (1+ pos2)))
    (remove* "" (append res  (list (substring s pos))) :test 'string=)
    )
  )

; (assert (equal (splitf "foo,bar,baz") (list "foo,bar,baz")))
; (assert (equal (car (splitf "\"Nqpbpx, Wvz\"	1661	R140\n\"Ntarj, Pelfgny\"	3200	J134\n")) "\"Nqpbpx, Wvz\"	1661	R140"))
; (assert (= 2 (length (splitf "\"Nqpbpx, Wvz\"	1661	R140\n\"Ntarj, Pelfgny\"	3200	J134\n"))))


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
  "maybe chop trailing end-of line (lf or crlf)"
  (cond ((not (string* s)) s)
	(t
	 (let* ((cr ?\r)
		(lf ?\n)
		last)
	   (cond
	    ((and (> (length s) 1) (eq (setq last (aref (substring s -2) 0)) cr))
	     (substring s 0 -2))
	    ((and  (> (length s) 1) (eq last lf))
	     (substring s 0 -2))
	    ((eq (aref (substring s -1) 0) lf)
	     (substring s 0 -1))
	    (t s))
	   )
	 )
	)
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

(defun read-file (f &optional chomp)
  "returns contents of FILE as a string
with optional second arg CHOMP, applies `chomp' to the result
" 
  (and f (file-exists-p f)
       (with-temp-buffer
	 (insert-file-contents f)
	 (if chomp
	     (chomp (buffer-string))
	   (buffer-string)
	   ))
       )
  )

(provide 'cat-utils)
