(put 'ctl-ret 'rcsid
 "$Id: ctl-ret.el,v 1.1 2010-04-17 17:55:03 keystone Exp $")

(unless (fboundp 'ctl-RET-prefix) 
    (define-prefix-command 'ctl-RET-prefix))

(unless (and (boundp 'ctl-RET-map) ctl-RET-map)
  (setq ctl-RET-map (symbol-function 'ctl-RET-prefix)))

; apparently C-RET is not a good prefix key if you're on telnet session
(if window-system
    (if (and (boundp 'running-xemacs) running-xemacs)
	(global-set-key (vector '(control return)) 'ctl-RET-prefix)
      (global-set-key  (vector 'C-return) 'ctl-RET-prefix))
  (global-set-key "\C-j" 'ctl-RET-prefix)
  )

(provide 'ctl-ret)
