(put 'whence 'rcsid 
 "$Id$")

; just to remove dependency on ... whencepath
(defvar whence-executable-terminator (if (eq window-system 'w32) "\\(.exe\\)?$" "$") "regexp to match executable files")

(defun whence (thing &optional path)
  "return location of THING along optional PATH
if specified, PATH is a list of directories.  default is split $PATH
"
  (loop 
   with y = nil
   with l = exec-path
   with pat = (concat "^" thing whence-executable-terminator)
   for x in l when (setq y (directory-files x t pat)) return (car y)
   )
  )

; (whence "pwd")
(provide 'whence)
