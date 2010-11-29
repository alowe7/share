; directory-files appears to have a bug matching arbitrary regexps.

(defun get-directory-files (&optional directory full match)
  "return directory contents as a list of strings, excluding . and ..
see `directory-files'
"
  (interactive "sName: ")

  (loop for x in 
	(directory-files (or directory ".") full match)
	when 
	(let ((z (file-name-nondirectory x)))
	  (not (or (string= z ".") (string= z ".."))))
	collect x)
  )

(provide 'directories)
