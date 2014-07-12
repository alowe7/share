; yet another prefix map

(unless (fboundp 'alt-SPC-prefix) 
    (define-prefix-command 'alt-SPC-prefix))

(unless (and (boundp 'alt-SPC-map) alt-SPC-map)
  (setq alt-SPC-map (symbol-function 'alt-SPC-prefix)))

(global-set-key  " " 'alt-SPC-prefix)

(provide 'alt-spc)
