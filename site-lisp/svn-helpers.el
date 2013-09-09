(put 'svn-helpers 'rcsid
 "$Id$")

(require 'long-comment)
(condition-case err
    (require 'semantic/wisent/comp)
  (error (message "warning: error requiring 'semantic/wisent/comp"))
  )

(/*
use Pod::Usage;
=pod

=head1 NAME

	svn-command-xml

=head1 DESCRIPTION

	evaluate svn command, returning results as xml
	note: discards stderr, so some important info might be lost...
	

=head1 NAME

	svn-get-item

=head1 DESCRIPTION

	
	

=head1 NAME

	svn-get-property

=head1 DESCRIPTION

	get the name of svn PROPERTY on ITEM
	if property is not nil, just return that
	else item defaults to `buffer-file-name'
	if interactive prompt for property among properties existing on item.
	else choose the first property on item, if any.
	

=head1 NAME

	svn-pl

=head1 DESCRIPTION

	return a list of svn properties on item
	

=head1 NAME

	svn-pl*

=head1 DESCRIPTION

	return a-list of svn (property value) on item
	

=head1 NAME

	svn-pg

=head1 DESCRIPTION

	return value of property on item
	

=head1 NAME

	svn-command

=head1 DESCRIPTION

	evaluate svn command, returning results as xml
	

=head1 NAME

	svn-ps

=head1 DESCRIPTION

	set svn PROPERTY on ITEM to NEW-VALUE
	

=head1 NAME

	svn-pe

=head1 DESCRIPTION

	edit svn PROPERTY on ITEM
	when called interactively will prompt for property, item and open an `xa' edit window to edit the property value.
	do nothing if user cancels out of `xa'  
	
	when called from `dired-mode' or `locate-mode' item defaults to selected files, else item defaults to `buffer-file-name'
	if only one property exists on the specified item, use that as default property.
	

=head1 NAME

	dired-pl

=head1 DESCRIPTION

	
	

=head1 NAME

	dired-pg

=head1 DESCRIPTION

	
	

=head1 NAME

	dired-edit-property

=head1 DESCRIPTION

	
	

=head1 NAME

	dired-edit-svn-ignore

=head1 DESCRIPTION

	
	

=head1 NAME

	svn-info-as-list

=head1 DESCRIPTION

	
	

=head1 NAME

	insertp

=head1 DESCRIPTION

	insert THING if PREDICATE holds.
	thing is not evaluated unless predicate
	

=head1 NAME

	rcsid-pattern-for-mode

=head1 DESCRIPTION

	
	

=head1 NAME

	rcsid-for-mode

=head1 DESCRIPTION

	
	

=head1 NAME

	svn-identify

=head1 DESCRIPTION

	like `identify' insert a sccs ID string at head of file, if not already there.
	but also ensure svn:keywords property contains Id
	

=head1 NAME

	svn-keyword-cleanup

=head1 DESCRIPTION

	return a list of items in current directory that do not have keyword property on them with value containing Id
	


=cut
*/)


(require 'cl)
(require 'xml)
(require 'dom)
(require 'xpath-helpers)
(require 'zap)

(defun svn-command-xml (&rest args)
  "evaluate svn command, returning results as xml
note: discards stderr, so some important info might be lost...
"
  (with-current-buffer (get-buffer-create " *svn output*")
    (erase-buffer)
    (let* ((*svn-stderr* (make-temp-file "svn-error"))
	   (ret (apply 'call-process (append `("svn" nil (t ,*svn-stderr*) nil) (remove nil args) '("--xml"))))
	   (errmsg (when (file-exists-p *svn-stderr*) (read-file *svn-stderr*)))
	   )

      (when (file-exists-p *svn-stderr*)
	(delete-file *svn-stderr*))

      (cond
       ((string* errmsg) (error errmsg))
       (t (dom-make-document-from-xml
	   (car (xml-parse-region (point-min) (point-max)))))
       )
      )
    )
  )

; (svn-command-xml "pl" "*")

(defun svn-get-item (&optional item)
  (let ((item  (cond
		(item item)
		((eq major-mode 'dired-mode) (dired-what-file))
		((eq major-mode 'locate-mode) (locate-get-filename))
		(t (buffer-file-name)))))
    (assert (not (null (string* item))))
    item)
  )
; (svn-get-item)

(defvar *svn-default-properties* '("svn:keywords") "list of properties known to svn by default")
(defvar *svn-default-directory-properties* '("svn:ignore") "list of properties known to svn by default applicable to directories")

(defun svn-get-property (&optional property item)
  "get the name of svn PROPERTY on ITEM
if property is not nil, just return that
else item defaults to `buffer-file-name'
if interactive prompt for property among properties existing on item.
else choose the first property on item, if any.
"
  (interactive)

  (or property
      (let ((pl (union (svn-pl (svn-get-item item)) (append (and (file-directory-p item)  *svn-default-directory-properties*) *svn-default-properties*))))

	(if (interactive-p) (completing-read "property: " pl)
	  (car pl)))

      )
  )
; (call-interactively 'svn-get-property)
; (svn-get-property "foo")

(defun svn-pl (&optional item)
  "return a list of svn properties on item
"
  (interactive
   (list 
    (svn-get-item))
   )

  (let* 
      ((item (svn-get-item item))
       (svn-document
	(svn-command-xml "pl"  item))
       (svn-properties
	(and svn-document (dom-document-get-elements-by-tag-name svn-document 'property)))
       (ret (and svn-properties
		 (loop for x in svn-properties collect
		       (xpath-attribute x "name")
		       ))))

    (and ret (cond ((interactive-p) (message (replace-regexp-in-string "\n" " " (pp ret)))) (t ret)))
    )
  )
; (svn-pl)

(defun svn-pl* (&optional item)
  "return a-list of svn (property value) on item
"
  (interactive
   (list 
    (svn-get-item)))

  (let* 
      ((item (svn-get-item item))
       (svn-document
	(svn-command-xml "pl"  item))
       (svn-target
	(car (dom-document-get-elements-by-tag-name svn-document 'target)))
       (svn-properties
	(and svn-target (xpath-resolve svn-target "property")))
       (ret
	(and svn-target svn-properties 
	     (cons (expand-file-name (xpath-attribute svn-target "path"))
		   (loop for x in svn-properties collect (let ((property-name (xpath-attribute x "name"))) (cons  property-name (svn-pg property-name item))))
		   ))))
    (cond ((interactive-p) (message (replace-regexp-in-string "\n" " " (pp ret)))) (t ret))
    )
  )
; (svn-pl*)

(defun svn-pg (&optional property item)
  "return value of property on item
"
  (interactive)
  (let* 
      ((item (svn-get-item item))
       (property (svn-get-property property))
       (svn-pg-document
	(svn-command-xml "pg" property item))
       (svn-property-node
	(car (dom-document-get-elements-by-tag-name svn-pg-document 'properties)))
       (svn-property-value-node
	(and  svn-property-node (xpath-resolve svn-property-node "/target/property")))
       )
    (and svn-property-value-node
	  (dom-node-text-content (car svn-property-value-node)))
    )
  )
; (svn-pg "svn:ignore" "/home/a/emacs/lisp")
; (svn-pg "svn:keywords")
; (svn-pg "svn:ignore")

(defun svn-command (&rest args)
  "evaluate svn command, returning results as xml
"
  (let ((b  (zap-buffer" *svn output*")))
    (with-current-buffer b
      (apply 'call-process (append `("svn" nil t nil) args))
      )
    )
  )


(defun svn-ps (property item new-property-value)
  "set svn PROPERTY on ITEM to NEW-VALUE
"
  (svn-pe property item new-property-value)
  )

(defun svn-pe (&optional property item new-property-value)
  "edit svn PROPERTY on ITEM
when called interactively will prompt for property, item and open an `xa' edit window to edit the property value.
do nothing if user cancels out of `xa'  

when called from `dired-mode' or `locate-mode' item defaults to selected files, else item defaults to `buffer-file-name'
if only one property exists on the specified item, use that as default property.
"
  (interactive)
  (let* (
	 (item (svn-get-item item))
	 (property (svn-get-property property item))
	 (original-property-value (svn-pg property item))
	 (new-property-value (or new-property-value (and (interactive-p) (xa (format "%s.%s" item property) original-property-value))))
	 )

    (unless (or
	     (null new-property-value) ; new-property-value will be null if xa cancelled out
	     (string= new-property-value original-property-value))
      (svn-command "propset" property new-property-value item))
    )
  )
; (svn-pe)

; "/home/a/emacs/lisp"
(defun dired-pl ()
  (interactive)
  (message (mapconcat 'identity (svn-pl (expand-file-name (dired-what-file))) " "))
  )

(defun dired-pg (&optional property)
  (interactive)
  (let* ((item (expand-file-name (dired-what-file)))
	 (property (or property (and (interactive-p)
				     (completing-read "edit property: " (svn-pl item))))))
    (assert property)
    (message (svn-pg property item))
    )
  )


(defun dired-edit-property (&optional property)
  (interactive)

  (let* ((item (expand-file-name (dired-what-file)))
	 (property (or property (and (interactive-p)
				     (completing-read "edit property: " (union (svn-pl item) *svn-default-properties*))))))
    (assert property)
    (svn-pe property item)
    )
  )

(defun dired-edit-svn-ignore ()
  (interactive)
  (dired-edit-property "svn:ignore")
  )

(defun svn-info-as-list (&optional dir)

  (let* ((default-directory (expand-file-name (or dir default-directory)))
	 (temporary-file-directory (getenv "TMP"))
	 (f (make-temp-file "svn-info"))
	 (cmd (format "svn info -R %s --xml" default-directory))
	 )

    (write-region (eval-process cmd) nil f)

    (xml-parse-file f)
    )
  )

(/*
; (setq x (svn-info-as-list "~/emacs/lisp"))

(loop for x in (get-directory-files default-directory nil ".el")
      collect (eval-process (format "svn pl %s" x)))


for a in *.el; do x=`svn pl $a`;  echo $x | grep -q svn:executable; if  [ $? -eq 1 ]; then echo $a; fi; done
*/)

(defmacro insertp (thing predicate)
  "insert THING if PREDICATE holds.
thing is not evaluated unless predicate
"
  `(if ,predicate (insert ,thing))
  )
; (insertp (progn (read-string "foo") "me") (read-string* "peace is for everyone: "))

(defun rcsid-pattern-for-mode ()
  (if (eq major-mode 'emacs-lisp-mode)
      "(put '\\w* 'rcsid"
    (concat (or comment-start "#") "\$Id:")
    )
  )

(defun rcsid-for-mode ()
  (if (eq major-mode 'emacs-lisp-mode)
      (format "(put '%s 'rcsid\n \"\$Id\$\")\n" 
	      (intern (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (format "%s \$Id\$ %s\n" (or comment-start "#") (or comment-end ""))
    )
  )

(defun  svn-identify () 
  "like `identify' insert a sccs ID string at head of file, if not already there.
but also ensure svn:keywords property contains Id
"
  (interactive)

  (let* ((svn-properties (svn-pl))
	 (svn-keyword-property "svn:keywords"))
    (cond
     ((or (null svn-properties) (not (member  svn-keyword-property  (svn-pl))))
  ; no properties or svn:keyword is not among them.  
      (svn-ps svn-keyword-property nil "Id"))
     (t
      (let ((svn-keywords 
	     (svn-pg "svn:keywords")))

	(cond
  ; svn:keyword existed with null value
	 ((null (string* svn-keywords)) (svn-pe "svn:keywords" nil "Id"))
  ; svn:keyword existed but did not have Id
	 ((not (string-match "Id" svn-keywords)) (svn-pe "svn:keywords" nil (concat svn-keywords " Id")))
  ; else do nothing
	 )
	)
      )
     )
    )

  (save-excursion
    (goto-char (point-min))
    ;; quote dollars to avoid keyword expansion here
    (insertp (rcsid-for-mode) (not (looking-at (rcsid-pattern-for-mode))))
    )
   
  )

; 


(defun svn-keyword-cleanup ()
  "return a list of items in current directory that do not have keyword property on them with value containing Id
"
  (interactive)  
  (let* 
      ((svn-document
	(svn-command-xml "pl"  "*"))
       (svn-targets
	(dom-document-get-elements-by-tag-name svn-document 'target))
       (result
	(remove* nil
		 (loop for node in svn-targets collect
		       (let (
			     (path (xpath-attribute node "path"))
			     (svn-keyword-property "svn:keywords" )
			     (properties (xpath-resolve node "property"))
			     )
			 (unless
			     (and
			      (member svn-keyword-property
				      (loop for x in properties collect (xpath-attribute x "name"))
				      )
			      (string-match "Id" (svn-pg svn-keyword-property path))
			      ) 
			   path)
			 )
		       )
		 )))
    (if (interactive-p)
	(let ((b (zap-buffer "foo")))
	  (set-buffer b)
	  (loop for x in result do (princ x) (princ "\n"))
	  (switch-to-buffer b)
	  (fb-mode)
	  )
      result)
    )
  )

; (call-interactively 'svn-keyword-cleanup)


(define-key vc-prefix-map "p" (lambda () (interactive) (message (chomp (pp (svn-pl (thing-at-point 'file)))))))

(provide 'svn-helpers)
