(put 'python-process 'rcsid
 "$Id$")

(require 'eval-process)

(defvar *python-command* (executable-find "python") "where to find python executable") 
(unless (file-executable-p *python-command*) (error "python executable not found: %s" *python-command*))

(defvar *python-log-command* nil)

(defun eval-python-process (script &rest args)
  (when *python-log-command* (message "%s %s %s" *python-command* script (mapconcat 'identity args " ")))
  (apply 'eval-process `(,*python-command* ,script ,@args))
  )
; (eval-python-process script args)

(defun* pipe-string-to-process (stuff command &rest args)
  "send STUFF via stdin to COMMAND with optional args
returns output from command, if any
"

  (when *python-log-command* (message "%s %s" command (mapconcat 'identity args " ")))

  (let* (
	 (bname (concat command "-pipe-buffer"))
	 (b 
	  (progn (and (get-buffer bname) (kill-buffer bname)) (get-buffer-create bname)))
	 (c (nconc (list
		    (concat command "-pipe-process")
		    b
		    command)
		   args))
	 (p (apply 'start-process c))
	 )

    (unless (and (processp p) (eq (process-status p) 'run)) (error "process %s died" command))

  ; gobble process status changes
    (set-process-sentinel p (function (lambda (process event) (if (string-match "exited abnormally" event) (error event)))))

    (process-send-string p stuff)
    (process-send-eof p)
    (accept-process-output p)

    ; wait as briefly as possible
    (loop for i from 1 to 1000 unless (eq (process-status p) 'exit) do (sit-for 0.1))

    (if (and (processp p) (eq (process-status p) 'run)) (error "process %s did not die" command))

    (unless (= (process-exit-status p) 0) 
      (error  "process %s died with exit code %d.  %s" command (process-exit-status p)     
	      (with-current-buffer b (buffer-string))))

    (with-current-buffer b (chomp (buffer-string)))
    )
  )

(defun keyword-args-to-switches (argmap)
  "ARGMAP is a  is an alist of the form: ((--switchname value) ...)
this function returns a string suitable for  appending to a command line.
switch is ommitted where value is nil
switch is retained plain where value is t
else switch is retained with following value

for example :
	(let ((foo 1) (bar \"baz\") (bo t)) (keyword-args-to-switches `((--foo ,foo) (--bar ,bar) (--bo ,bo) (--buzz nil))))

returns:
	 '(\"--foo\" \"1\" \"--bar\" \"baz\" \"--bo\")))

"

   (remove* nil
	    (apply 'nconc
		   (mapcar
		    (function (lambda (x) (cond ((listp x)  (cond ((eq (cadr x) t) (list (format "%s" (car x)))) ((cadr x) (mapcar (function (lambda (y) (and y (format "%s" y)))) x)))) (x (format "%s" x)))))
		    argmap)
		   )
	    )
  )
; (assert  (equal (let ((foo 1) (bar "baz") (bo t)) (keyword-args-to-switches `((--foo ,foo) (--bar ,bar) (--bo ,bo) (--buzz)))) '("--foo" "1" "--bar" "baz" "--bo")))
; (let ((regex "foo") id limit since prior) (keyword-args-to-switches `(("--regex" ,regex) ("--key" ,id) ("--limit" ,limit) ("--since" ,since) ("--prior" ,prior))))
; (let (regex id limit since prior) (keyword-args-to-switches `(("--regex" ,regex) ("--key" ,id) ("--limit" ,limit) ("--since" ,since) ("--prior" ,prior))))


(provide 'python-process)
