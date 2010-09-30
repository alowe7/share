(put 'roll 'rcsid 
 "$Id: roll.el,v 1.2 2010-09-30 00:05:01 keystone Exp $")
(provide 'roll)
(require 'buffers)
(require 'cl)
(require 'completion)

(defmacro roll (l)
  "destructively roll LIST as a ring"
  (if (symbolp l)
      `(setq ,l (nconc (copy-list (cdr ,l)) (list (car ,l))))
    `(nconc (copy-list (cdr ,l)) (list (car ,l))))
  )
; (roll (roll (roll '(a b c))))
; (progn (setq a '(a b c)) (roll a))

(defmacro roll-1 (l)
  "destructively roll LIST as a ring"
  (if (symbolp l)
      `(setq ,l (nconc (last ,l) (butlast ,l)))
    `(nconc (last ,l) (butlast ,l)))
  )
; (roll-1 '(a b c))
; (progn (setq z '(a b c))  (roll-1 z))
; (roll '(a b c))
; (roll (roll (roll '(a b c))))
; (let ((a '(a b c))) (roll a))

(defun roll-search (a pat displayfn i)
  (position pat a 
	    :test 
	    '(lambda (a b) (string-match a (if displayfn (funcall displayfn b) b)))
	    :start
	    (or i 0)
	    ))


(defvar roll-nav-map ()
  "a-list of named  keys to functions.
this might make it easier to adjust navigation through the list by redefining nav-map."
  )

(setq roll-nav-map '(
		     (back 
		      ?p ? ?)
		     (delete
		      ?d)
		     (help
		      ??)
		     (search
		      ?/)
		     (quit
		      ?q)
		     (next
		      ?\ )
		     )

      )

(defun roll-nav (key fn)
  (member key (cdr (assoc fn roll-nav-map)))
  )

(defun roll-dispatch (key dispatch-map a i)
  (let ((fn (assoc key dispatch-map)))
    (and fn (cadr fn) (apply (cadr fn) (list a i)))
    fn
    )
  )

; (describe-key (vector (roll-nav (y-or-n-q-p "" "p?? ") 'next)))


(defun roll-list (l  &optional displayfn deletefn selectfn dispatch donefn)
  "rolls elements in list L, using DISPATCH list.

optional DISPATCH is an a-list: 
	((char fn) ...)

each association dispatches a keyboard character to a function.  if
the key is pressed, the function of one arg is applied to the list: (v i)
where v is the vector of l and i is the relative position of the subject element in v

the dispatch function may (throw 'done nil) to exit the input loop.

calling DISPLAYFN to display the element (returns a string)
	if DISPLAYFN is nil, then l is assumed to be a list of strings, and the element is simply displayed
calling DELETEFN to delete an element
	if DELETEFN is nil, then deleting from l has no side effects
calling SELECTFN to choose one
	if SELECTFN is nil, then roll-list simply returns the selected value
"

  (let* ((a (apply 'vector l))
	 (len (length a))
	 (i 0)
	 (last-pat "")
	 (b (catch 'done
	      (while (> len 0)
		(if (< i 0) (setq i (1- len)))
		(let* ((bb (aref a i))
		       (name (cond (displayfn (funcall displayfn bb)) ((stringp bb) bb) ((symbolp bb) (symbol-name bb)) (string* (condition-case err (format "%s" bb) (error nil)) "*unprintable*")))
		       (v (y-or-n-q-p ; name can have formatting characters in it
			   (replace-regexp-in-string "%" "%%" name) 
			   (concat "dp/\C-m ?"
				   (apply 'vector (loop for x in dispatch collect (car x))))
			   )))

		  (cond 

		   ((roll-nav v 'search)
		    (let* ((p (read-string (format "search for (%s): " last-pat)))
			   (pat (if (< (length p) 1) last-pat p))
			   (start (if (< (length p) 1) (1+ i) 0))
			   (next (roll-search a pat displayfn start)))
		      (if next (setq i next
				     last-pat pat)
			(message "%s not found" pat)
			(sit-for 2))))

		   ((roll-nav v 'help)
		    (progn
		      (message "roll-buffer: RET: goto; BS,p: back; SPACE,n: forward; DEL,d: delete; ?: help; q: quit")
		      (read-key-p)
		      (setq i (1- i))
		      ))

		   ((roll-nav v 'delete)
		    (if deletefn 
			(funcall deletefn l bb))
		    (setq a (apply 'vector l)
			  len (length a)
			  i (1+ i))
		    )

		   ((roll-nav v 'back) 
		    (setq i (1- i)))

  ; check for dispatch functions
		   ((and dispatch (roll-dispatch v dispatch a i))
		    nil)

  ; all other keys are handled here
		   ((and v (not (roll-nav v 'next)))
		    (throw 'done (and (not (roll-nav v 'quit)) bb)))

		   (t (setq i (1+ i))))
		  )
								
		(if (>= i len) (setq i 0))
		)))
	 ret)
    (if b (setq ret (if selectfn (funcall selectfn b) b)))

  ; in case a changed, allow it to restore some global state
    (if donefn (funcall donefn a))

    ret
    )
  )


(defvar roll-mode nil)

(defun kill-buffer-1 (l b)
  (kill-buffer b)
  (delete* b l)
  )

; treat list r as a ring and roll it 
(defun roll-ring (r) (nconc (cdr r) (list (car r))))

(defun roll-buffer-mode (arg)
  "apply `roll-list' to buffers in `collect-buffers-mode' using current `major-mode'
with prefix arg, prompts for major mode (completing from `atoms-like' \"-mode\")
"
  (interactive "P")

  (let* ((m 
	  (cond ((and (symbolp arg) (not (null arg))) arg)
		(arg (completing-read (format "mode (%s): " major-mode)
				      (mapcar '(lambda (x) 
						 (cons
						  (format "%s" x) x))
					      (atoms-like "-mode"))))
		(t major-mode)
		))
	(mode (if (string* m) (intern m) m))
	(l (roll-ring (collect-buffers-mode mode)))
	)

; ensure current buffer isn't at the front of the list
    (roll-list l 'buffer-name 'kill-buffer-1 'switch-to-buffer
	       '((?o (lambda (v i) (switch-to-buffer-other-window (aref v i)) (throw 'done nil)))))

    )
  )


(defun roll-buffer-mode-1 (&optional arg)
  "apply `list-mode-buffers' to specified major mode (in `atoms-like' \"-mode\")
see `roll-list'
applies `switch-to-buffer' as displayfn
"
  (interactive "P")
  (let ((m (if arg
	       (intern (completing-read "mode: " (mapcar '(lambda (x) 
							    (cons
							     (format "%s" x) x))
							 (atoms-like "-mode"))))
	     major-mode)))
    
    (roll-list (collect-buffers-mode (if (atom m) m (or (string* m) major-mode))) '(lambda (x) (progn (switch-to-buffer x) (buffer-name x))) 'kill-buffer-1 'switch-to-buffer)
    )
  )

(defun read-mode (prompt)
  "read something that might be a mode with completion"
  (let ((s (completing-read prompt (mapcar '(lambda (x) 
					      (cons
					       (format "%s" x) x))
					   (atoms-like "-mode$")))))
    (and (string* s) (intern s)))
  )

(defun roll-buffer-with-mode (pat &optional mode)
  "apply `roll-list' to buffers containing PAT and with optional MODE
if MODE is nil, uses current `major-mode' 
applies `switch-to-buffer' as displayfn
"
  (interactive (list
		(read-string "pat: ")
		(read-mode  "mode: ")))
  (roll-list (collect-buffers-with-mode pat (or mode major-mode))
	     '(lambda (x) (progn (switch-to-buffer x) (buffer-name x)))
	     'kill-buffer-1
	     'switch-to-buffer)
  )

(defun roll-buffer-like (arg) 
  " roll buffers with mode like current buffer"
  (interactive "P") 
  (let ((displayfn (if arg '(lambda (b) (switch-to-buffer b) (message (buffer-name b))) 'buffer-name)))
    (roll-list (roll (collect-buffers-mode major-mode)) displayfn 'kill-buffer-1 'switch-to-buffer
	       '((?l (lambda (v i) (message "%d/%d" i (length x)) (sit-for 2))))
	       )
    )
  )

(defun real-buffer-list (&optional arg)
  "returns `buffer-list' excluding killed buffers & those with names beginning with a space
with optional ARG, returns in reverse order
"
  (let* ((rbl (buffer-list))
	 (bl (append (cdr rbl) (list (car rbl))))
	 bn val x)
    (dolist (x (if arg bl (reverse bl)))
      (setq bn (buffer-name x))
  ;skip killed buffers & those whose name begins with a space
      (and bn (> (length bn) 0) (not (eq ?\  (aref bn 0))) (push x val)))
    val))

(defun* buffer-list-2 (&key mode named in modified notmodified withpat)
  ; (assert (or (not (or modified notmodified)) (not (and modified notmodified))))

  (loop for x being the buffers
	when
	(and (or (not mode)
		 (eq (progn (set-buffer x) major-mode) mode))
	     (or (not named)
		 (string-match named (buffer-name x)))
	     (or (not in)
		 (let ((d (if (buffer-file-name x) (buffer-file-name x) (save-excursion (set-buffer x) default-directory))))
		   (and d (string-match in d))))
	     (or (not modified)
		 (buffer-modified-p x))
	     (or (not notmodified)
		 (not (buffer-modified-p x)))
	     (or (not withpat)
		 (string-match withpat (save-excursion (set-buffer x) (buffer-string)))))
	collect x)
  )

(defun roll-buffer-list (&optional l) 
  "roll visible buffers.  if optional LIST is specified, use that as the list of buffers to roll"
  (interactive)
  (roll-list (or l (real-buffer-list nil)) 'buffer-name 'kill-buffer-1 'switch-to-buffer)
  )

(defun roll-buffer-list-1 (mode) 
  "like roll-buffer-list, but only list buffers in mode"
  (interactive (list 
  ; complete with any symbol that ends in "-mode"
		(complete* "list buffers in mode (%s): " "-mode$" (or roll-mode major-mode))
		))
  (roll-list
   (collect-buffers
    (cond
     ((and (stringp mode) (> (length mode) 0)) 
      (intern mode))
     ((or (stringp mode) (not mode) roll-mode) roll-mode)
     (t (setq roll-mode major-mode)))
    )
   'buffer-name 'kill-buffer 'switch-to-buffer)
  )

(defun roll-buffer-list-2 (l) 
  "like `roll-buffer-list`, but require LIST
LIST may be an a-list, in which case, interpret the cars as buffers, and print the cadrs as lables"
 
  (loop for x in l
	with b = nil
	with m = nil
	do
	(setq b (if (listp x) (car x) x))
	(setq m (if (listp x) (cadr x) ""))
	(set-buffer b)
	(message "%s -- %s" (buffer-name) m)
	(let ((c (read-char)))
	  (cond 
	   ((eq c ?\C-m) (return (pop-to-buffer b)))
	   ((eq c ?o) (return (switch-to-buffer-other-window b)))
	   ((eq c ?/) (return (pop-to-buffer b)))
	   )
	  )))

(defun roll-buffer-no-files (&optional mugger) (interactive "smodified? ") 
  (roll-list (collect-buffers-no-files (string* mugger))
	     nil
	     'kill-buffer-1 
	     'switch-to-buffer)
  )


(defun roll-buffer-named (&optional pat)
  "roll buffers with names matching PAT"
  (interactive)

  (let ((pat (string* pat (file-name-nondirectory (buffer-file-name)))))
    (roll-buffer-list (collect-buffers-named pat))
    )
  )

(defvar *last-roll-with* nil)
(defun roll-buffer-with (pat)
  "roll buffers with contents matching PAT"
  (interactive (list (string*
		      (read-string (format "roll buffer with (%s): " (string* *last-roll-with* (indicated-word))))
		      (or *last-roll-with* (indicated-word)))))
  (let ((l (collect-buffers-with pat)))
    (if l 
	(roll-list (or l (real-buffer-list nil)) 'buffer-name 'kill-buffer-1 '(lambda (b) (switch-to-buffer b)(goto-char (point-min)) (search-forward pat nil t)))
      (error (format "no buffers contain %s" pat))
      )
    )
  (setq *last-roll-with* pat)
  )


(defun first-mode (mode)
  "return first buffer in given mode"
  (interactive "smode: ")
  (loop for x in (real-buffer-list) thereis (progn (set-buffer x) (and (eq major-mode mode) x)))
  ) 

(defun first-shell () (interactive) (let ((b (first-mode 'shell-mode))) (if b (switch-to-buffer b) (message "not found"))))


(defun roll-buffer-list-2 (mode named in modified notmodified withpat)
  (interactive
   (let* ((mode (intern (completing-read "mode: " (mapcar '(lambda (x) (list (symbol-name x) x)) (symbols-like "-mode$")))))
	  (named (string* (read-string "named: ")))
	  (in (string* (read-string "in: ")))
	  (modified (not (not (string* (read-string "modified: ")))))
	  (notmodified (and (not modified) 
			    (not (not (string* (read-string "notmodified: "))))))
	  (withpat (string* (read-string "withpat: "))))
     (list mode named in modified notmodified withpat)))
  (let ((fn '(lambda (x)
	       (setq fn 
		     '(lambda (x)
			(switch-to-buffer x)
			(if withpat (search-forward withpat))
			(buffer-name x)))
	       (switch-to-buffer-other-window x)
	       (if withpat (progn (goto-char (point-min)) (search-forward withpat)))
	       (buffer-name x))))
    (roll-list
     (buffer-list-2 
      :mode mode 
      :named named
      :in in
      :modified modified  
      :notmodified notmodified
      :withpat withpat)
     '(lambda (x) (funcall fn x)) 'kill-buffer-1 '(lambda (x) (message (buffer-name x)))
     )
    )
  )

(fset 'qurol 'roll-buffer-list-2)

; (qurol 'shell-mode nil "/l" nil nil "insight")
; (qurol "emacs-lisp-mode" nil nil nil nil "doit")
; (qurol "" nil nil nil nil "doit")
; (roll-list (buffer-list-2 :mode 'shell-mode :in "/l") 'buffer-name)
