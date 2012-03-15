(put 'lib 'rcsid
 "$Id$")

; basic functions that override or extend builtins 

; directory-files appears to have a bug matching arbitrary regexps.

(defun get-directory-files (&optional directory full match predicate)
  "return directory contents as a list of strings, excluding . and ..
see `directory-files'
"
  (interactive "sName: ")

  (loop
   with dir = (or directory ".")
   for x in 
   (directory-files dir full match)
   when 
   (and 
    (let ((z (file-name-nondirectory x)))
      (not (or (string= z ".") (string= z ".."))))
    (or (not predicate) (funcall predicate x dir)))
   collect x)
  )

(provide 'directories)
