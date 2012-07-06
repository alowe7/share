(add-to-list 'load-path ".")
(require 'collect-directories)
;(loop for x in (collect-directories (expand-file-name "lisp/cedet" source-directory)) do (add-to-list 'load-path x))
(setq byte-compile-warnings '(not cl-functions free-vars unresolved obsolete))
