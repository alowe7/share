(put 'whence 'rcsid 
 "$Id: whencepath.el 8 2010-10-05 01:38:57Z svn $")

; just to remove dependency on ... whencepath
(defun whence (thing &optional path)
  "return location of THING along optional PATH
if specified, PATH is a list of directories.  default is split $PATH
"
  (loop 
   with y = nil
   with l = (split-string (getenv "PATH") path-separator t)
   with pat = (concat thing "$")
   for x in l when (setq y (get-directory-files x t pat)) return (car y)
   )
  )

(provide 'whence)
