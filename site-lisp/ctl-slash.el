; yet another prefix map
(if (not (fboundp 'ctl-/-prefix)) 
    (define-prefix-command 'ctl-/-prefix)) ;; don't wipe out map if it already exists

(global-set-key (vector (ctl ?/)) 'ctl-/-prefix)

(setq ctl-/-map (symbol-function  'ctl-/-prefix))

(provide 'ctl-slash)
