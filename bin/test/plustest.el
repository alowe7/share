; autoload test-explicit-autoload
(defun* test-defun* () )
(defun test-defun () )
(defmacro test-defmacro () )
(define-derived-mode child parent test-derived-mode)
(fset test-fset '(lambda () "I smell smoke"))
(defalias test-alias '(lambda () "I smell smoke"))
