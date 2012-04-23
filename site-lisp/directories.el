(put 'lib 'rcsid
 "$Id$")

; basic functions that override or extend builtins 

; directory-files appears to have a bug matching arbitrary regexps.

(defun get-directory-files (&optional directory full match predicate)
  "Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
if PREDICATE is non-nil, it is a predicate of two arguments: (filename directory) returning non-nil if argument is to be included in resule

this function differs from `directory-files' in the nature of the predicate argument and that it excludes dot (.) and dotdot (..) by defauls
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
