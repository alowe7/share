(put 'fapropos 'rcsid 
 "$Id$")

(require 'indicate)

(defvar fapropos-alist '((emacs-lisp-mode obarray)
			 (lisp-interaction-mode obarray)
			 (c++-mode c-obarray )
			 (html-mode c-obarray )
			 (shell-mode shell-obarray)))

(defun fapropos-mode ()
  (goto-char (point-min))
  (help-mode)
  (set-buffer-modified-p nil)
  (setq truncate-lines t)
  )

(defun fapropos1 (pat default-array)
" return matches for PAT in ARRAY formatted as a single string"
  (let ((val))
    (dolist (a	default-array)
      (if (arrayp  (if (symbolp a) (eval a) a))
	  (dotimes (x (length (if (symbolp a) (eval a) a))) 
	    (let ((s (format "%s" (aref (if (symbolp a) (eval a) a) x))))
	      (if (string-match pat s) (setq val (concat val s "\n")))))
	(dolist (x (if (symbolp a) (eval a) a))
	  (if (string-match pat (car x)) (setq val (concat val (car x) "\n"))))
	)
      )
    val))

(defun fapropos4-helper ()
  (nconc (list
	  (read-string "pattern: ")
	  (eval (intern (string* (read-string "obarray: ")))))
	 (let (x) (loop
		   while (string* (setq x (read-string "pattern: ")))
		   collect x)))
  )

(defun fapropos4 (pat array &rest args)
  "return matches for PAT in ARRAY"
  (interactive (fapropos4-helper)) ;; if specified remaining args are joined

  (let ((array (or array obarray)))
    (loop 
     for x across array
     if (and (symbolp x) (string-match pat (symbol-name x))
	     (or (not args)
		 (loop for y in args
		       unless (string-match y (symbol-name x)) return nil
		       finally return t)
		 )
	     )
     collect x)
    )
  )


(defvar *obstack* '(obarray))

(defun push-obstack (x)
  (interactive (list (completing-read "obarray: " '(("obarray") ("c-obarray")) nil t)))
  (setq *obstack* (eval (intern x))))


(defun fapropos (pat &rest array)
  "a version of apropos that actually works.
default behavior is to search all obarrays in `*obstack*' for REGEXP.
interactively with prefix arg, searches emacs obarray.
called from a program, optional args are:
REGEXP OBARRAY
if interactive and REGEXP is not given, it is prompted for.
else if REGEXP is not given, (indicated-word) is used
fapropos will only find symbols which have already been interned
"
  (interactive (op-arg  "fapropos (%s): "))
  (let* ((default-array 
	   (or array (cdr (assoc major-mode fapropos-alist))
	       *obstack*))
	 (b (zap-buffer "*Help*"))
	 (w (get-buffer-window b))
	 (val (and pat (> (length pat) 0) (fapropos1 pat default-array)))
	 )

    (if val
	(progn
	  (setq *obstack* default-array)
	  (or (and w (select-window w))
	      (switch-to-buffer-other-window b))
	  (insert val)
	  (goto-char (point-min))
	  )
      (message "no matches for %s in %s" pat (car default-array))
      )
    )
  )

(defun atoms-like (y)
  (loop
   for x being the symbols of obarray 
   if (and (or (boundp x) (functionp x)) (string-match y (format "%s" x)))
   collect x))


(defun fapropos2 (x) (interactive "sstring: ")
  (loop
   initially (zap-buffer "*Help*")
   finally (progn (pop-to-buffer "*Help*") (goto-char (point-min)))
   for y in (atoms-like x) do
   (let ((s (format "%s" y))) 
     (insert s "\t" 
	     (or (if (functionp y)
		     (documentation y)
		   (documentation-property y 'variable-documentation)) "") "\n"))))

(defun symbols-like (s)
  "list variables with names matching regexp PAT"
  (loop for sym being the symbols
	when (or (boundp sym) (fboundp sym))
	when (string-match s (symbol-name sym))
	collect sym)
  )
; (symbols-like "goto")

(defun collect-modes ()
  (symbols-like "-mode$")
  )

(defun functions-like (pat)
  "list functions with names matching regexp PAT"
  (interactive "sString: ")
  (sort (loop for x being the symbols 
	      when (and (string-match pat (symbol-name x))
			(condition-case nil (symbol-function x) (error nil))) 
	      collect x) 'string-lessp)
  )

(defun functions-like* (&rest args)
  "apply `functions-like' to all elements of LIST"
  (let ((l (functions-like (pop args))))
    (loop for x in args do 
	  (setq l (intersection l (functions-like x))))
    l)
  )

; (functions-like* "buffer" "window")

(defun autoloaded-functions (&optional pat )
  "return a list of symbols bound to autooaded functions
with optional PATTERN, return matching symbols
"

  (loop for x being the symbols 
	when
	(and (or (not pat) (string-match pat (symbol-name x)))
	     (functionp x)
	     (condition-case nil (eq (car (symbol-function x)) 'autoload)  (error nil))) 
	collect x)
  )
; (autoloaded-functions)
; (loop for x in (autoloaded-functions "zt") do (fmakunbound x))


; (add-hook 'completion-setup-hook 'apropos-completion-setup-function)

; (defun apropos-completion-setup-function ()
;  (local-set-key (vector (quote down-mouse-1))
; 								(lambda (e) (interactive) (debug))))

(defvar *fapropos3-cache* nil)

(defun fapropos3 (s) (interactive "sShow symbols matching regexp: ")
"Show all symbols whose names match REGEXP."
  (let* ((ss (completing-read "Complete: " 
			      (loop for x in (setq *fapropos3-cache* (symbols-like s))
				    collect
				    (list x)) nil t))
	 (ssi (intern ss)))

    (cond ((fboundp ssi) 
	   (describe-function ssi))
	  ((boundp ssi)
	   (describe-variable ssi))
	  )
    )
  )

(defun refine-apropos (s) (interactive "sString: ")
  "Show all subset of last `fapropos3' call that also match REGEXP."
  (let* ((ss (completing-read "Complete: " 
			      (loop for x in *fapropos3-cache*
				    when (string-match s x)
				    collect
				    (list x)) nil t))
	 (ssi (intern ss)))

    (cond ((fboundp ssi) 
	   (describe-function ssi))
	  ((boundp ssi)
	   (describe-variable ssi))
	  )
    )
  )


(defun member-1 (s l &optional depth)
  "determine if string S is a member of list L descending to optional DEPTH."
  (cond ((stringp l) 
	 (and (string-match s l) 0))
	((or (null l)
	     (not  (listp l)))
	 nil)
	((not (numberp depth))
	 (loop for w in l thereis (member-1 s w depth)))
	((> depth -1)
	 (loop for w in l thereis (member-1 s w (1- depth))))
	(t nil))
  )

; (member-1 "a" '("a") 0)
; (let ((*apropos-string-lists-with* 1)) (call-interactively 'vars-with))

(defvar *apropos-string-lists-with* 1 "include lists of strings in `vars-with'")

(defun vars-with (s)
  "list variables where value is a string matching regexp PAT
if the variable `*apropos-string-lists-with*' is set, then include lists of strings containing PAT
if `*apropos-string-lists-with*' is a number, don't descend into lists any deeper than that
"
  (interactive "sString: ")
  (let ((v (loop for sym being the symbols
		 when (boundp sym)
		 when (or (and (stringp (eval sym)) (string-match s (eval sym)))
			  (and *apropos-string-lists-with* (listp (eval sym)) 
			       (member-1 s (eval sym) 
					 (and (numberp  *apropos-string-lists-with*)  *apropos-string-lists-with*))))
  ; specifically exclude target
		 unless (string-match "^s" (symbol-name sym) )
		 collect sym)))
    (if (interactive-p)
	(let* ((b (zap-buffer "*vars*"))
	       (standard-output b))
	  (loop for x in v
		do 
		(insert "\n" (symbol-name x) "\n" "\t")
		(pp (eval x))
		(insert "\n")
		)

	  (pop-to-buffer b)
	  (fapropos-mode)
	  ))
    v)
  )

(defun vars-like (name)
  "list variables where symbol-name matches regexp NAME"
  (interactive "sname like: ")
  (let ((v (loop for sym being the symbols
		 when (boundp sym)
		 when (stringp (eval sym))
		 when (string-match name (symbol-name sym))

  ; specifically exclude target, because, of course the symbol 'name
  ; will match while we're in this loop...
		 unless (string-match "^name$" (symbol-name sym) )

		 collect sym)))

    (if (interactive-p)
	(let* ((b (zap-buffer "*vars*"))
	       (standard-output b))
	  (loop for x in v
		when (boundp x) ; not sure why this test is necessary again
		do 
		(insert "\n" (symbol-name x) "\n" "\t")
		(pp (eval x))
		(insert "\n")

		)
	  (pop-to-buffer b)
	  (fapropos-mode)))
    v)
  )

(defun vars-like-with (name pat)
  "list variables where symbol-name matches regexp NAME, 
and value is a string matching regexp PAT"
  (interactive "sname like: \nsvalue like: ")
  (let ((v (loop for sym being the symbols
		 when (boundp sym)
		 when (stringp (eval sym))
		 when (string-match name (symbol-name sym))
		 when (string-match pat (eval sym))

  ; specifically exclude target, because, of course the symbol 'name
  ; will match while we're in this loop...
		 unless (string-match "^name$" (symbol-name sym) )

		 collect sym)))

    (if (interactive-p)
	(let* ((b (zap-buffer "*vars*"))
	       (standard-output b))
	  (loop for x in v
		do 
		(insert "\n" (symbol-name x) "\n" "\t")
		(pp (eval x))
		(insert "\n")

		)
	  (pop-to-buffer b)
	  (fapropos-mode)))
    v)
  )


(defun vars-like-with-expr (pat &optional l)
  "list variables where name is a string matching regexp PAT, 
and value satisfies lambda expression L.
L takes a single arg, the name of the var unevaluated"
  (interactive "ssymbol names matching: \nxstring values matching (exprs ok):")
  (let ((v (loop for sym being the symbols
		 when (boundp sym)
		 when (string-match pat (symbol-name sym))
		 when (or (not l) (funcall l sym))
		 collect sym)))
    (if (interactive-p)
	(let ((m ""))
	  (loop for x in v
		do (setq m (concat m (symbol-name x) " ")))
	  (message m)))
    v)
  )


(defun function-help-string (fn &optional more)  
  "displays the help string for FUNCTION.
with optional arg MORE, appends its value to the help string,
unless it is already there
"
  (let* ((f (symbol-function fn)) 
	 (s (caddr f)))
    (and (stringp s) 
	 (or (and (null more) s)
	     (and (null (string-match more s))
		  (rplaca (cddr f) (concat s more)))))))
; e.g. 
; (function-help-string  'cscope-mode "  t to toggle tpath searching
;  d to debug tpath searching
;")


; xxx todo roll results
(defun documentation-like (pat)
  "find symbol function with documentation property matching PAT"
  (interactive "spat: ")
  (loop
   for x being the symbols of obarray 
   when (and (functionp x) (documentation x) (string-match pat (documentation x)))
   collect x
   )
  )

(defun describe-key-sequence (&optional arg) (interactive "P") (funcall (if arg 'describe-key 'describe-key-briefly) (read-key-sequence "key sequence: ")))

(defun fapropos5 (pat)
  "get help for functions with documentation matching PAT"
  (interactive "spat: ")
  (let ((b (zap-buffer "*Help*")))
    (set-buffer b)
    (loop for x in (documentation-like pat) do
	  (insert (format "`%s'\t%s\n\n" x (documentation x)))
	  )
    (pop-to-buffer b)
    (help-mode)
    (goto-char (point-min))
    )
  )
;(fapropos5 "load-path")

(provide 'fapropos)
