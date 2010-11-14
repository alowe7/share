(put 'php-mode 'rcsid
 "$Id$")

(define-derived-mode php-mode java-mode "PHP" "")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(modify-syntax-entry ?_ "w" php-mode-syntax-table)

(provide 'php-mode)
