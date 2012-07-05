(require 'ctl-meta)

; yet another prefix map
(if (not (fboundp 'ctl-comma-prefix)) 
    (define-prefix-command 'ctl-comma-prefix)) ;; don't wipe out map if it already exists

(global-set-key (vector (ctl ?,)) 'ctl-comma-prefix)

(defvar ctl-comma-map (symbol-function  'ctl-comma-prefix))

(provide 'ctl-comma)

