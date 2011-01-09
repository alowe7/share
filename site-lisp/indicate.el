(put 'indicate 'rcsid 
 "$Id$")

(require 'typesafe)
(require 'thingatpt) ; which see

;;
;; returns the word before point.
;; use modify-syntax-entry to change the definition of a "word"
;; 

;; alternate way to push/pop syntax class:
;; (comma-syntax
;;  (prog1 
;; 		 (char-syntax ?,)
;; 	 (modify-syntax-entry ?, "w")
;; 	 (indicated-word)
;; 	 (modify-syntax-entry ?, comma-syntax)
;; 	 ))
;; 

(defvar *indicated-word-region* nil 
  "holds region containing most recent indicated-word.
see `indicated-word-region'")

;; this may be too round-about, but the original implementation set off an undebuggable time-bomb 

(defun indicated-word (&optional include-chars from to exclude-chars)
  "evaluates to word indicated by cursor
   if string  INCLUDE-CHARS is specified, 
temporarily change the syntax entry for each char in the string to \"w\"
in the current buffer
" 
  (interactive)

  (let* ((old-table (syntax-table))
	(new-table (copy-syntax-table old-table))
	w)

    (save-restriction 
      (narrow-to-region (or from (point-min)) (or to (point-max)))

      (set-syntax-table new-table)

      (loop for char across include-chars do
      	    (modify-syntax-entry char "w"))
      (loop for char across exclude-chars do
      	    (modify-syntax-entry char "."))

      (setq *indicated-word-region* (bounds-of-thing-at-point 'word)
	    w (if *indicated-word-region*
		  (buffer-substring (car *indicated-word-region*) (cdr *indicated-word-region*))
		""))

      (set-syntax-table old-table)

      (if (interactive-p) (message w) w)
      )
    )
  )



(defun indicated (&optional thing include-chars exclude-chars from to)
  "evaluates to THING indicated by cursor
see `bounds-of-thing-at-point' for possible values of thing.
   if string  INCLUDE-CHARS is specified, 
temporarily change the syntax entry for each char in the string to \"w\"
see `modify-syntax-entry' for possible values for that.
in the current buffer
" 
  (interactive)

  (let* (
	 (thing (or thing 'symbol))
	 (old-table (syntax-table))
	 (new-table (copy-syntax-table old-table))
	 (include-syntax-char (cond ((eq thing 'symbol) "_")
				    (t "w")))
	 (exclude-syntax-char ".")
	 w)

    (save-restriction 
      (narrow-to-region (or from (point-min)) (or to (point-max)))

      (set-syntax-table new-table)

      (loop for char across include-chars do
      	    (modify-syntax-entry char include-syntax-char))
      (loop for char across exclude-chars do
      	    (modify-syntax-entry char exclude-syntax-char))

      (setq *indicated-word-region* (bounds-of-thing-at-point thing)
	    w (if *indicated-word-region*
		  (buffer-substring (car *indicated-word-region*) (cdr *indicated-word-region*))
		""))

      (set-syntax-table old-table)

      (if (interactive-p) (message w) w)
      )
    )
  )

(defun op-arg (prompt &rest args)
  "acquire an argument using prompt.  default to indicated word.
prompt may have %s format specifications to show that
indicated-word is the default. remaining args are applied to prompt"
  (list
   (let ((w (read-string (apply 'format (append (list prompt (indicated-word)) args)))))
     (or (and (> (length w) 0)  w) (indicated-word))
     ))
  )

; also see comint-file-name-chars
(defvar *filename-chars* (if (eq window-system 'w32)  ":/.\\"   "/."))

(defun indicated-filename ()
  "evaluates to filename indicated by cursor"
 (indicated-word *filename-chars*)
)

(defun find-indicated-file () 
  "reads in filename indicated by cursor"
  (interactive)
  (find-file (indicated-filename))
  )

(defun word ()
  "displays (indicated-word)"
  (interactive)
  (message (indicated-word))
  )

;; interactively get a number from keyboard
(defun getcvt (v)
  (let ((e 1) (n 0))
    (while v (setq n (+ n (* (pop v) e)) e (* e 10)))
    n)
  )
;; todo : rewrite to use last-input-char rather than arg?
(defun getnumber (&optional arg)
  "arg is optional first digits as a list"
  (let ((v arg))
    (catch 'done
      (while t
	(let
	    ((x  (- (read-char) ?0)))
	  (if (or (> x 9) (< x 0)) (throw 'done v))
	  (push x v)
	  (message "%d" (getcvt v)) ;echo prompt
	  )
	)
      )
    (getcvt v)
    )
  )


(defun indicated-line ()
  "returns current line"
  (interactive)
  (let (b e) 
    (save-excursion
      (beginning-of-line)
      (setq b (point))
      (end-of-line)
      (setq e (point))
      (buffer-substring b e))))


(defmacro complete-indicated-word (prompt table &rest args)
  "complete indicated word using PROMPT and completing in TABLE.
prompt may contain formatting chars which as substituted via format
first with (indicated-word), then remaining ARGS.

a typical use might be with (interactive) :

  (interactive 
   (list (complete-indicated-word \"lookfor (%s): \" completion-table)))

"

  `(let ((*i* ,(indicated-word))
	 (*rest* ,@args))
     (string* (completing-read (apply 'format ,prompt (cons *i* *rest*)) ,table) *i*)
     )
  )
; (complete-indicated-word "you are here (%s) %s: " '("able" "baker" "charlie" "delta") '("not"))asdf

(defun search-forward-indicated-word () 
  (interactive)
  (search-forward (indicated-word))
  )

(defun replace-indicated-word (expr) (interactive "xExpr: ")
  "replace indicated word with results of evaluating EXPR
EXPR may be a function of zero or one argument, the indicated-word"
  (let ((x (indicated-word)))
    (apply 'delete-region *indicated-word-region*)
    (if expr
	(condition-case err
	    (insert (funcall expr x))
	  (wrong-number-of-arguments
	   (unless (cadr (cadr err)) ; allow if expr takes no args
	     (caddr (cadr err))))
	  )
      )
    )
  )

; for example, to double a list of numbers:
;(while (not (eobp))
;  (replace-indicated-word '(lambda (x) 
;			     (format "%d" (* (read x) 2))))
;  (forward-line 1)
;  (beginning-of-line)
;
;  )

;; hm.  see `current-word'
(defun read-indicated-string (prompt)
  "read a string with PROMPT default to `indicated-word'"
  (interactive "sprompt: ")
  (let ((s (indicated-word))) (string* (read-string (format "%s (%s): " prompt s)) s))
  )

(fset 'indicated 'thing-at-point)

(provide 'indicate)
