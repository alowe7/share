(put 'ctl-backslash 'rcsid
 "$Id$")

(if (not (fboundp 'ctl-\\-prefix)) 
    (define-prefix-command 'ctl-\\-prefix));; don't wipe out map if it already exists

(global-set-key "\C-\\" 'ctl-\\-prefix)

(defvar ctl-\\-map (symbol-function  'ctl-\\-prefix))

(provide 'ctl-backslash)
