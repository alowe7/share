(put 'nums 'rcsid 
 "$Id$")

(require 'eval-process)

(defun exp (n m)
  " compute n ** m "
  (if (= m 0) 1
    (let ((s "(* "))
      (while (> m 0) 
	(setq s (concat s (int-to-string n) " "))
	(setq m (1- m)))
      (eval (read  (concat s ")")))
      )
    )
  )

(defun hex (s)
  " convert integer arg S as hex formatted string
S may also be a string representation of a decimal number "
  (interactive "sdecimal number: ")
  (let ((h (format "0x%0x" (if (integerp s) s (string-to-number s)))))
    (if (interactive-p) 
	(message h) h))
)

(defun dec (s &optional m)
" return decimal equivalent of hex string S
  with optional second arg, display result.
" 
  (interactive "shex number: ")
  (let* ((s (cond ((stringp s) s) ((atom s) (symbol-name s)) ((integerp s) (format "%d" s))))
	 (al '(("0" . 0) ("1" . 1) ("2" . 2) ("3" . 3) ("4" . 4) ("5" . 5) ("6" . 6) ("7" . 7) ("8" . 8) ("9" . 9) 
	       ("a" . 10) ("b" . 11) ("c" . 12) ("d" . 13) ("e" . 14) ("f" . 15)
	       ("A" . 10) ("B" . 11) ("C" . 12) ("D" . 13) ("E" . 14) ("F" . 15)
	       ))
	 (v 0) (p 0) (b 16) (n (length s)))

    (while (> n 0)
      (let
	  ((c (substring s (1- n) n)))
	(if 
	    (eq (string-to-char c) (string-to-char "x"))
	    (setq n 0)
	  (progn
	    (setq v (+ v 
		       (* (cdr (assoc c al)) (exp b p))
		       )
		  )
	    (setq p (1+ p))
	    (setq n (1- n))
	    )
	  )
	)
      )
    (if (or (interactive-p) m) (message "%d" v) v)
    )
  )

(defun oct (s)
  "interpret string ARG as an octal number and return the integer equivalent base 10
arg may be a number or a string.
"
  (interactive "soctal: ")
  (let ((v (string-to-number (if (integerp s) (format "%d" s ) s) 8)))
    (if (interactive-p) (message (format "%d" v)) v)
    )
  )

(defun indicated-float ()
  "convert indicated word from a hex number to floating point"
  (interactive)
  (message (clean-string (eval-process "a2f" (indicated-word))))
  )

(defun insert-indicated-float ()
  "convert indicated word from a hex number to floating point"
  (interactive)
  (insert (format " %s " (clean-string (eval-process "a2f" (indicated-word)))))
  (kill-word 1)
  )

(defun show-float (&optional arg)
  "show hex representaion of indicated floating point number
with arg, prompt for number.
"
  (interactive "P")
  (message (clean-string (eval-process "f2a" (or (and arg (read-string "fp: ")) (indicated-word)))))
  )


(defun indicated-dec ()
  "convert indicated word from a hex number to decimal"
  (interactive)
  (dec (indicated-word) t))


(defun indicated-hex ()
  "convert indicated word from a decimal number to hex"
  (interactive)
  (hex (indicated-word)))


(defun ascii (s)
  "s is a string representing a sequence of hex numbers seperated by whitespace.  
   return the corresponding ascii string
   null terminates"
  (interactive "s")
  (let ((c nil)
	(ns ""))
    (while (and (> (length s) 0) (not (equal c "")))
      (setq c (format "%c" (dec (substring s 0 2))))
      (setq ns (concat ns c))
      (if (< (length s) 3)
	  (setq s "")
	(setq s (substring s 2 (length s))))
      )
    (if (called-interactively-p 'any) (message ns) ns)
    )
  )

(defun ascii-region (beg end)
  "convert region of hex numbers to corresponding ascii string"
  (interactive "r")
  (ascii (clean-string (buffer-substring beg end)))
  )

(defun ascii2 (s)
  "s is a string. return the ascii of the hex version of the string"
  (while (> (length s) 0)
    (insert (format "%x"  (string-to-char s)))
    (setq s (substring s 1 (length s)))))

(defun ascii-region2 (beg end)
  "convert region of text to hex"
  (ascii2 (buffer-substring beg end)))

(defvar *number-lines-separator* ". ")
(defun number-lines-separator (sep)
  (interactive "sseparator for number-lines: ")
  (setq *number-lines-separator* sep)
  )

(defun number-lines (&optional beg end base column separator zeros)
  " insert line numbers in region starting at optional number BASE.
  if called interactively line numbers are inserted at the current column (default 0)
interactively with arg, arg specifies the starting point

when called from a program, arguments are BEG END BASE COLUMN SEPARATOR ZEROS
BEG and END are character positions defining the operand region 
BASE indicates the starting line number
COLUMN defines where on the line the number appears
SEPARATOR seperates the number from the line (default: `*number-lines-separator*')
ZEROS if set, line number includes leading zeros
"
  (interactive "P")
  (save-excursion
    (let* 
	((i (cond ((and (interactive-p) (not (null base)) (listp base)) (car base))
		  ((numberp base) base)
		  (t 1)))
	 (p (if (interactive-p) (min (point) (mark)) (min beg end)))
	 (q (if (interactive-p) (max (point) (mark)) (max beg end)))
	 (z (+ i (count-lines p q)))
	 (goal-column (or column goal-column (current-column)))
	 (separator (or separator *number-lines-separator*))
	 (iformat (if zeros "%03d" "%d"))
	 )

      (goto-char p)
      (move-to-column goal-column t)

      (while (< i z)
	(insert (concat (format iformat i) *number-lines-separator*)) 
	(forward-line 1)
	(setq i (1+ i))
	)
      )
    )
  )

(defun denumber-lines (beg end)
  "remove line numbering from lines in region"

  (interactive "r")

  (goto-char beg)
  (while (re-search-forward  "[0-9]+\\)\\.? ?" end t)
    (replace-match "" nil nil))

  )


(defun ascii-word (arg)
  "point is on a word of ascii text. decode it.
with arg, use region."
  (interactive "P")
  (let ((s (if arg 
	       (replace-letter (buffer-substring (point) (mark)) " " nil)
	     (indicated-word)))
	l s2) 
    (while (> (length s) 0) 
      (let* ((len (length s))
	     (m (min 2 len))) 
	(push (format "%c" (dec (substring s 0 m))) l) 
	(setq s (substring s m (length s)))))
    (while l (setq s2 (concat (pop l) s2)))
    (message s2)))

(if (boundp 'shell-mode-map)
(define-key shell-mode-map "a" 'ascii-word))

(defun parse-rgb (c)
"format for rgb value is #rrggbb"
(let ((rr (format "%c%c" (aref c 1) (aref c 2)))
      (gg (format "%c%c" (aref c 3) (aref c 4)))
      (bb (format "%c%c" (aref c 5) (aref c 6))))
(list (dec rr) (dec gg) (dec bb))
))


; (loop for x in  (parse-rgb "#F5E39E") do (insert (format "%d " x)))
; 245 227 158 

(provide 'nums)
