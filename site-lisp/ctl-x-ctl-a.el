(put 'ctl-x-ctl-a 'rcsid
 "$Id$")

(unless (fboundp 'ctl-x-ctl-a-prefix) 
    (define-prefix-command 'ctl-x-ctl-a-prefix))

(defvar ctl-x-ctl-a-map (symbol-function 'ctl-x-ctl-a-prefix))

(global-set-key "" 'ctl-x-ctl-a-prefix)

(provide 'ctl-x-ctl-a)


