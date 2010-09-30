(put 'completion 'rcsid
 "$Id: completion.el,v 1.1 2010-09-30 00:05:01 keystone Exp $")

(defmacro complete*  (prompt &optional pat default)
  "read a symbol with completion.
 prompting with PROMPT, complete in obarry for symbols matching regexp PAT,
 default to DEFAULT"  
  (let ((sym (completing-read  
	      (format prompt (eval default))
	      obarray
	      (if pat (lambda (x) (string-match pat (format "%s" x)))))))
    (if (and (sequencep sym) (> (length sym) 0)) sym default))
  )
