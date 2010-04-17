(put 'fb 'rcsid 
 "$Id: fb.el,v 1.1 2010-04-17 17:55:03 keystone Exp $")

(require 'locate)
(require 'isearch)
(require 'cat-utils)
(require 'qsave)
(require 'trim)
(require 'indicate)
(require 'scratch-mode)

(defvar *fb-case-fold* t)
(defvar *fb-show-lines* t)
(defvar *fb-auto-go* t)
(defvar fb-load-hook nil)

(defvar fb-mode-hook nil)
(defvar fb-last-pat nil)
(defvar fb-last-match nil)

(defvar *fastfind-buffer* "*ff*")

(defconst *default-fb-db* 
  (or (getenv "FBDB")
      "/var/spool/f")
  "cache of working file list.")

(defvar *fb-db* 
  (or (getenv "FBDB")
      "/var/spool/f")
  "cache of working file list.")


;; these add qsave capability to fb-search buffer
(defvar *find-file-query* nil)
(defun find-file-save-search ()
  (qsave-search (current-buffer) *find-file-query* default-directory)
  )

; probably should rename as after-fb-find-file-hook
(defvar after-find-file-hook nil)
(add-hook 'after-find-file-hook 'find-file-save-search)

(defun fb-match-file (pat &optional direction)
  "find file matching PAT in optional DIRECTION"
  (if (> (length pat) 0) 
      (setq fb-last-match nil
	    fb-last-pat (replace-in-string "*" "[^\n]*" pat)))
  (fb-search nil direction)
  )

(defun fb-match-file-forward (pat) 	
  (interactive 
   (list	(read-string (format "pat (%s): " fb-last-pat))))
  (fb-match-file pat)
  )

(defun fb-match-file-backward (pat) 	
  (interactive "spat: ")
  (fb-match-file pat t)
  )

(defun fb-search (spat &optional backwards)

  (and spat (setq fb-last-match nil
		  fb-last-pat spat))

  (if (and (not backwards)
	   fb-last-match
	   (eq (point) (car fb-last-match)))
      (goto-char (cdr fb-last-match)))
 
  (let ((pat (if (> (length spat) 0) spat fb-last-pat))
	(search (if backwards 're-search-backward 're-search-forward))
	(bound  (if backwards 'match-end 'match-beginning)))

    (if (funcall search pat nil t)
	(setq fb-last-match (cons (match-beginning 0)  (match-end 0)))
      (progn
	(setq fb-last-match nil)
	(message "%s not found."  pat))
      )
    )
  )

; sometimes filelists include letter drive names like c:/foo/bar, sometimes they contain line numbers like /foo/bar:20
(defun fb-indicated-file ()
  (interactive)
  (let* ((s (bgets))
		 (l (split s ":"))
		 (file (elt l 0))
		 (line (int* (elt l 1)))
		 (v (trim-white-space (if line file s)))
		 )
	(if (interactive-p)
		(message v))
	v)
  )
(fset 'fb-get-filename 'fb-indicated-file)


(defun fb-indicated-line ()
  (let* ((s (bgets))
	 (l (split s ":"))
	 (file (elt l 0))
	 (line (int* (elt l 1))))
    (or line 0))
  )

(defun fb-dired-file (&optional arg)
  "run dired on indicated file. with optional ARG, dired containing directory
"
  (interactive "P")
  (let ((f (fb-indicated-file)))
    (dired (or (and arg (file-name-directory f)) f))
    )
  )

(defun fb-delete-file ()
  (interactive)
  (let ((f (fb-indicated-file)))
    (unless (not (y-or-n-p (format "delete file %s? " f)))
      (delete-file (fb-indicated-file))
      (let ((x buffer-read-only)) 
	(setq buffer-read-only nil)
	(delete-region
	 (progn (beginning-of-line) (point)) 
	 (progn (forward-line 1) (point)))
	(set-buffer-modified-p nil)
	(setq buffer-read-only x)
	)
      )
    (message "")
    )
  )

(defun fb-dired-file-other-window ()
  (interactive)
  (dired-other-window (fb-indicated-file))
  )

(defun fb-find-file ()
  (interactive)
  (find-file (fb-indicated-file))
  )


(defun fb-dired-file-1 ()
  (interactive)

  (let* ((l (split (bgets) ":"))
	 (file (elt l 0))
	 (b (dired-noselect file)))
    (switch-to-buffer b)
    )
  )

(defun fb-find-file-other-window ()
  (interactive)
  (find-file-other-window (fb-indicated-file))
  )

(defun fb-exec-file (&optional arg)
  (interactive "P")
  (aexec (fb-indicated-file) arg)
  )

(defun fb-do-shell-command (command)
  (interactive (list (read-string (format "! on %s: " (fb-indicated-file)))))
  (shell-command (format "%s %s" command (fb-indicated-file)) (get-scratch-buffer " "))
  )

(defun fb-w3m-file ()
  (interactive)
  (or (featurep 'w3m) (require 'w3m))
  (let ((f (fb-indicated-file)))
    (w3m-goto-url (format "file://%s" 
		      (if (string-match "[a-z]:" f)
			  (substring f (match-end 0)) f)))
    )
  )

(defun fb-pod-file ()
  (interactive)
  (fb-shell-command "pod2text")
  )

(defun fb/ () 
  "produce recursive dired like listing on slash.
see variable *fb-db* "
  (interactive)
  (let ((b (find-file-read-only *fb-db*)))
    (pop-to-buffer b)
    (cd-absolute (expand-file-name "/"))
    (fb-mode)

    )
  )

(defun fb (&optional buf)
  "view file listing from current directory"
  (interactive)
  (let* ((b (or buf
	       (get-buffer-create
		(generate-new-buffer-name
		 (expand-file-name (pwd))))))
	(whatever (shell-command "find . -type f -print" b))
	(w (get-buffer-window b)))
    
    (or (and w (select-window w))
	(switch-to-buffer-other-window b))

    (beginning-of-buffer)
    (fb-mode))
  )

; xxx smart.
(and (boundp 'dired-mode-map)
     (define-key dired-mode-map "" 'fb))

(defun fb-search-forward (pat) 
  (interactive 
   (list (read-string (format "search for (%s): " (fb-indicated-file)))))

  (fb-search
   (if (> (length pat) 0) pat
     (fb-indicated-file))))

(defun fb-search-backward (pat) 
  (interactive 
   (list (read-string (format "search for (%s): " (fb-indicated-file)))))
  (fb-search
   (setq fb-last-match nil
	 fb-last-pat
	 (if (> (length pat) 0) pat
	   (fb-indicated-file))) t))


(defun fb-file-info () 
  "
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes.
  This is a floating point number if the size is too large for an integer.
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.  If inode number is larger than the Emacs integer,
  this is a cons cell containing two integers: first the high part,
  then the low 16 bits.
11. Device number.
"
  (interactive)
  (let* ((fn (fb-indicated-file))
	 (a (file-attributes fn)))
  ; -rw-r--r--   1 544      everyone     2655 Mar 28  1998 woody
    (if a
	(message "%s %-4d %-4d %s %d %s %s"

		 (elt a 8)
		 (elt a 3)
		 (elt a 2)
		 "everyone"
		 (elt a 7)
		 (format-time-string "%b %d %Y %H:%M:%S" (elt a 5))
		 (file-name-nondirectory fn)
		 )
      (message "file not found"))
    )
  )


(defun fb-pipe-command (cmd)
  (interactive "sshell command: ")

  (shell-command (format "%s < %s" cmd (fb-indicated-file)) (get-scratch-buffer " "))
  )



(define-derived-mode fb-mode fundamental-mode "Fb"
  "mode for managing file index.
like locate mode, with the following exceptions:
d	`fb-dired-file'
f	`fb-find-file'
RET	`fb-exec-file'
/	`fb-search-forward'
?	`fb-search-backward'
M-/	`fb-match-file-forward'
M-?	`fb-match-file-backward'
w	`fb-w3m-file'
P	`fb-pod-file'
|	`fb-grep-files'

also see `fb-mode-map'
"

  (set-buffer-modified-p nil)

  (setq mode-name "Fb")

  (setq major-mode 'fb-mode)

  ; for windows
  (setq default-directory (expand-file-name default-directory))

  (setq mode-line-process nil)
  (run-hooks 'fb-mode-hook)
  )


(define-key fb-mode-map "g" 
  '(lambda () (interactive)
     (let ((default-directory default-directory))
       (save-restriction
	 (widen)
	 (toggle-read-only -1)
	 (delete-region (point-min) (point-max)))
       (fb (current-buffer))
       )))
 
(define-key fb-mode-map "q" 
  '(lambda () (interactive)
     (kill-buffer (current-buffer))))

(define-key fb-mode-map "\C-m" 'fb-exec-file)

(define-key fb-mode-map "w" 'fb-w3m-file)
(define-key fb-mode-map "P" 'fb-pod-file)

(define-key fb-mode-map "o" 'fb-find-file-other-window)
(define-key fb-mode-map "f" 'fb-find-file)

(define-key  fb-mode-map "D" 'fb-dired-file-other-window)
(define-key fb-mode-map "d" 'fb-dired-file) 

(define-key  fb-mode-map "x" 'fb-delete-file)

(define-key fb-mode-map [prior] 'fb-previous)
(define-key fb-mode-map [next] 'fb-next)

(define-key fb-mode-map [up] 'fb-up)

(define-key fb-mode-map "/" 'fb-search-forward)
(define-key fb-mode-map "?" 'fb-search-backward)

(define-key fb-mode-map "/" 'fb-match-file-forward)
(define-key fb-mode-map "?" 'fb-match-file-backward)

(define-key  fb-mode-map "p" 'roll-qsave)
(define-key  fb-mode-map "n" 'roll-qsave-1)

(define-key fb-mode-map "i" 'fb-file-info)

(define-key fb-mode-map "|" 'fb-grep-files)
(define-key fb-mode-map "<" 'fb-pipe-command)
(define-key fb-mode-map "!" 'fb-do-shell-command)
(define-key fb-mode-map "m" '(lambda () (interactive) 
			       (fb-shell-command "nroff -man")))

(define-key fb-mode-map "\C-d" (lambda () (interactive) 
				 (let ((f (fb-indicated-file)))
				   (if (file-exists-p f)
				       (delete-file f)
				     (message (format "%s f does not exist" f)))
				   )
				 )
  )

(loop for x across "#:+./-_~!"
      do
      (modify-syntax-entry x "w" fb-mode-syntax-table)
      )

(if (eq window-system 'w32)
    (loop for x across " \\"
	  do
	  (modify-syntax-entry x "w" fb-mode-syntax-table)
	  )
  )


(defun fb-up () 
  " goto beginning of this directory"
  (interactive)
  (let ((f (concat "^" (substring (file-name-directory (fb-indicated-file)) 0 -1) "$")))
    (while 
	(re-search-backward f nil t)
      )
    )
  )


(defun fb-previous () 
  " goto beginning of this directory"
  (interactive)
  (if (bobp) nil
    (previous-line 1)
    (let ((f (concat "^" (substring (file-name-directory (fb-indicated-file)) 0 -1) "$")))
      (while 
	  (re-search-backward f nil t)
	)
      )
    )
  )

(defun fb-next () (interactive)
  " goto beginning of this directory"
  (let ((f (concat "^" (substring (file-name-directory (fb-indicated-file)) 0 -1))))
    (while 
	(re-search-forward f nil t)
      )
    (and (not (eobp))
	 (forward-line 1)
	 (beginning-of-line))
    )
  )



(defun ff-hack-pat (pat)
  " modify a regular expression with wildcards to match minimally. 
e.g. convert \"foo*bar\" to \"foo[^b]*bar\"
all other patterns (e.g. \"foo*\") remain unchanged.
"
  (let* ((sp 0)
	 (oldstr pat)
	 (newstr ""))
    (loop 
     while
     (string-match "*." oldstr)
     do
     (setq newstr
	   (concat newstr (substring oldstr 0 (match-beginning 0))
		   "[^" 
		   (substring oldstr (1+ (match-beginning 0)) (+ (match-beginning 0) 2))
		   "]*")
	   oldstr (substring oldstr (1+ (match-beginning 0))))
     finally return (concat newstr oldstr)
     )
    )
  )

(setq ff-hack-pat 'identity)
; (setq ff-hack-pat 'ff-hack-pat)
; this hack might be suitable for systems with letter drive names
; (setq ff-hack-pat '(lambda (pat) (let ((pat (if (eq (aref pat 0) ?^) (substring pat 1) pat))) (concat "^(.:)*" pat))))


; hack to avoid auto-going to a binary file
(require 'eval-process)
(defun probably-binary-file (f)
  (or (file-exists-p f) (error (format "File %s doesn't exist" f)))
  (let ((v (eval-process "head -4" f)))
    (loop for x across v thereis (or (= x 0) (>= x 127))))
  )

(defun multi-join (l)
  " multi-way join on set of files l.  
returns a filename containing results"
  (save-excursion
    (let ((fn1 (pop l))
	  (b (zap-buffer " *multi-join*")))
      (loop 
       for f in l
       do
       (call-process "join" nil
		     b
		     nil
		     f fn1)
       (set-buffer b)
       (write-file fn1)
       (erase-buffer)
       (set-buffer-modified-p nil)
       )
      (kill-buffer b)
      fn1)
    )
  )

; tbd promote/merge config/os/*/fb.el ...

(defun ff (pat)
  "fast find files -- search for file matching PAT in `*fb-db*'"

  (interactive "sfind files: ")
  (let* ((top default-directory)
	 (default-directory "/")
	 (b (zap-buffer *fastfind-buffer*))
	 f)

    (if (= (length *fb-db*) 0)
	(progn
	  (setq *fb-db* *default-fb-db*)
	  (setq top "/")
	  )

      (progn 
	(if (file-directory-p *fb-db*)
	    (setq *fb-db* (concat *fb-db* "/f")))
	(setq 
	 top (file-name-directory *fb-db*)
	 )
	)
      )

    (ff1 *fb-db* pat b top)

  ; try to avoid splitting (buffer-string) 

    (cond ((and *fb-auto-go* 
		(interactive-p) 
		(= (count-lines (point-min) (point-max)) 1)
		(not (probably-binary-file (setq f (car (split (buffer-string) "
"))))))
  ; pop to singleton if appropriate
	   (find-file f))
  ; else pop to listing if interactive
	  ((interactive-p)
	   (pop-to-buffer b))
  ; else just return the list
	  (t (split (buffer-string) "
")
	     ))
    )
  )

; e.g. find executable only:
;  (ff "lff" '(lambda (x) (and (string-match "x" (elt (file-attributes x) 8)) x)))
;  (ff "bin" '(lambda (x) (and (string-match "drwx" (elt (file-attributes x) 8)) x)))

(defun fb-grep-files (arg)
  "search for REGEXP in files in region
with prefix argument, prompt for additional args for grep
"
  (interactive "spat: ")
  (let* ((s 
	  (cond ((or (not (interactive-p)) (and arg (not (listp arg)))) arg)
		(t (read-string "Search for: "))))
	 (grep-command (if (and (interactive-p) arg (listp arg) (> (car arg) 1)) (read-string "grep command : " grep-command grep-command) grep-command))
	 (dir (if (string= "/" (buffer-substring (point-min) (1+ (point-min)))) "/" default-directory))
	 (p1 (point-min))
	 (p2 (point-max))
	 (b (let ((b (get-buffer-create "*grep*")))
	      (save-excursion
		(set-buffer b)
		(setq buffer-read-only nil)
		(erase-buffer)
		(compilation-mode))
	      b))
	 (err (get-buffer-create (generate-new-buffer-name "*Shell-Command-Error*")))
	 (resize-mini-windows nil)
	 )

; if grep-command is a form of grep add -s option to ignore missing files (if not already specified)
    (if (and (string-match "grep" grep-command)
	     (not (string-match "-s" grep-command)))
	(setq grep-command (replace-in-string "grep " "grep -s " grep-command)))

    (shell-command-on-region
     p1
     p2
     (format "xargs %s %s" grep-command s)
     b
     nil
     err
     )

    (cond
     ((save-excursion
	(set-buffer err)
	(> (length (buffer-string)) 1))
      (save-excursion
	(set-buffer err)
	(message (buffer-string))))
     ((and (interactive-p) 
	   (save-excursion
	     (set-buffer b)
	     (> (length (buffer-string)) 1)))
      (let ((w (get-buffer-window b)))
	(if w (select-window w)
	  (switch-to-buffer b)))
      ; insert grep-command locus so next-error will work
      (goto-char (point-min))
      (insert (format "cd %s\nxargs %s %s\n" dir grep-command s))
      )
     ((interactive-p) 
      (message "no matches found"))
     )
    )
  )

(provide 'fb)

(run-hooks 'fb-load-hook)

