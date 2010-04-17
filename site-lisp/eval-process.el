(put 'eval-process 'rcsid 
 "$Id: eval-process.el,v 1.1 2010-04-17 17:55:03 keystone Exp $")
;; a package to return process evaulation as a string

(require 'zap)
(require 'trim)

;; processes that return values

(defun default-directory* ()
  ; handle case where default-directory is unavailable for some reason
  (if (file-directory-p default-directory) default-directory (expand-file-name "/"))
  )

(defvar last-exit-status nil)

(defvar *eval-process-stderr-file* nil "path to contain stderr from eval-process")
; alternative is to (defun* eval-process .. &key *stderr* ...)

(defun eval-process (cmd &rest args)
  "execute CMD as a process, giving it optional ARGS.
CMD may be a string evaluating to a command, or a space separated list of strings indicating the command and arguments
ARGS may be a space separated string or a list of string arguments

Kludgy feature: performs `split' on cmd before calling `call-process', so 
	(eval-process \"command -switch arg1 arg2\")
is equivalent to 
	(eval-process \"command\" \"-switch\" \"arg1\" \"arg2\")
or even 
	(eval-process \"command -switch arg1\" \"arg2\")

this function evaluates to the process output  "
  (let*
      ((cmd 
	(let ((cmd1 (if (listp cmd) cmd (split cmd))))
	  (setq args (nconc (cdr cmd1) args))
	  (car cmd1)))
       (dir (default-directory*))
       (buffer (get-buffer-create " *eval*"))
       v)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq default-directory dir)
      (setq last-exit-status nil)
      (setq last-exit-status
	    (condition-case x
		(apply 'call-process (nconc (list cmd nil (list buffer *eval-process-stderr-file*) nil) args))
	      (error
	       (cond
		((eq 'file-error (car x))
		 (message (mapconcat 'identity (cddr x) ": ")))
		))
	      )
	    )
  ;    (set-buffer buffer)
  ;    (setq v (buffer-string))
  ;    (kill-buffer buffer) ; may be faster not to bother with this.
  ;    v
      (cond ((= 1 (count-lines (point-min) (point-max)))
	     (clean-string (buffer-string)))
	    (t (buffer-string)))
      )
    )
  )


(defun insert-eval-process (cmd &optional args)
  " insert results of executing COMMAND into current buffer
COMMAND may be the name of a command or include a space separated list of args
optional second arg ARGS may be a space separated list of additional args.
so
 (insert-eval-process \"ls\")
 (insert-eval-process \"ls -l\")
 (insert-eval-process \"ls\" \"-l\")
all work.
see `eval-process'
 "
  (interactive "scommand: ")
  (insert (funcall 'eval-process cmd args))
  )

;; todo -- rewrite to use &rest

(defun eval-string (cmd &optional arg)
	"special case of eval-process taking CMD and one ARG.
cleans up linefeeds in resulting output"
	(clean-string (eval-process cmd arg))
	)

(defun eval-shell-command (cmd)
  "execute string CMD as a shell. return results as a string"
  (let
      ((dir default-directory)
       (buffer (get-buffer-create " *eval*"))
       )
    (save-window-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq default-directory dir)
      (apply 'shell-command (list cmd buffer nil))
      (cond ((= 1 (count-lines (point-min) (point-max)))
	 (clean-string (buffer-string)))
	(t (buffer-string)))
      )
    )
  )
;; this is here because it is mostly used to clean up ^J's from eval-process output

(defun replace-letter (s old-letter &optional new-letter)
  (let* ((ol (if (and (stringp old-letter) (> (length old-letter) 0))
		(aref old-letter 0) old-letter))
	(nl (if (and (stringp new-letter) (> (length new-letter) 0))
		     (aref new-letter 0) new-letter))
	(fn (if nil
		'(lambda (x) (char-to-string (if (char-equal x ol) nl x)))
	      '(lambda (x) (if (char-equal x ol) nl (char-to-string x)))
	      )))
    (mapconcat fn s ""))
  )

;; this is no reason to (require 'cl)
;; (defun replace-letter (s old-letter &optional new-letter)
;;   "modify STRING replacing OLD-LETTER with NEW-LETTER" 
;;   (let ((ol (if (stringp old-letter) (aref old-letter 0) old-letter))
;; 	(nl (if (string* new-letter) (aref new-letter 0) new-letter)))
;; 
;;     (if nl
;; 	(loop for x across s 
;; 	      collect (if (eq x ol) nl x) into l
;; 	      finally return (concat (apply 'vector l)))
;;       (loop for x across s 
;; 	    when (not (eq x ol))
;; 	    collect x into l
;; 	    finally return (concat (apply 'vector l)))
;;       )
;;     )
;;   )

(defun clean-string (s &optional c)
  "replace occurrences of ^J in STRING with CHAR (default nil)"
  (trim (replace-letter s "
" c))
  )

;;; *** from c-fill.el ***

(defun re-clean-string (from s &optional to)
  "replace regexp FROM in string S to string TO (default nil)"
  (let ((pos 0)
	(new "")) 
    (while (string-match from s pos)
      (setq new (concat new (substring s pos (match-beginning 0)) to))
      (setq pos (match-end 0))
      )
    (concat new (substring s pos))
  )
)

(defun timestamp (&optional format)
  "add a fixed timestamp to the current buffer using time format string FORMAT.
see `format-time-string' and `time-stamp'"
  (interactive)
  (insert (format-time-string (string* format "%Y/%m/%d %T")))
  )

(defun cksum (f)
  "returns the checksum of FILE
see cksum(1)
"
  (interactive "ffile: ")
  (car (read-from-string (car
			  (loop for x in
				(split (eval-shell-command (format "cksum %s" f)))
				when (string* x)
				collect x)
			  )))
  )

(provide 'eval-process)
