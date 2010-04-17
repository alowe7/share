(put 'whencepath 'rcsid 
 "$Id: whencepath.el,v 1.1 2010-04-17 17:55:03 keystone Exp $")
(require 'sh)
(require 'cat-utils)
(require 'cl)

;; examples
;; (whencepath "ls")
;; (whencepath "other" "HOWTOPATH" t)

(defun whencepath (cmd &optional path regexp executable)
  " look for cmd along path (path is a list of strings)"
  (interactive "scmd: \nspath: ")
  (let* (
	 (default-directory (expand-file-name (getenv "SYSTEMDRIVE")))
	 (abscmd
	  (loop for x in (split-path (getenv (or path "PATH")))
		thereis
		(loop for fx in 
		      (nconc (list (concat x "/" cmd))
			     (unless (or
				      (not (eq window-system 'w32))
				      (string-match "\.exe$" cmd))
			       (list (concat x "/" cmd ".exe"))))
		      when
		      (if regexp
			  (loop for y in (get-directory-files x)
				thereis
				(and (string-match cmd y) (concat x "/" y))
				)
			(if executable
			    (if (-x fx) fx)
			  (if (-f fx) fx))
			)
		      return fx
		      )
		)
	  ))
    (and abscmd
	 (if (interactive-p) (message abscmd) abscmd))
    )
  )

;; examples
;; (whence  "src_my_pre_change")
;; (whence "binlog")

(defun whence (cmd)
  " find COMMAND along path"
  (interactive "scommand: ")
  (let ((abscmd (whencepath cmd "PATH"))
	(fc  (string-to-char cmd)))
    ;; check for shell function invocation (doesn't work for aliases)
    (if (or (not (string= abscmd cmd)) (char-equal fc ?/) (char-equal fc ?.))
	abscmd
      ;; I think this is a ksh function or alias. check along $FPATH
      (whencepath cmd "FPATH")
      )
    )
  )

; (whencepath "la")

(defun find-whence (cmd)
  " find COMMAND along path"
  (interactive "scommand: ")
  (let ((abscmd (whencepath cmd)))
    (if abscmd 
	(find-file abscmd) 
      (message "%s not found" abscmd)
      )
    )
  )

(provide 'whencepath)
