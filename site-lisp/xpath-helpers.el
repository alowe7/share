(put 'xpath-helpers 'rcsid
 "$Id$")

(defun xpath-attributes (node)
  "return cons of attribute name . value for all attributes on NODE
"
  (loop for x in (dom-node-attributes node)
	collect (cons (dom-attr-name x)  (dom-attr-value x))))

; (xpath-attributes svn-entry)

(defun xpath-attribute (node attribute)
  "return attribute value for attribute on NODE attribute name matching ATTRIBUTE
ATTR is a string with optional xpath syntax like \"@name\"
"
  (let ((attribute-name 
	 (if (string-match "^@" attribute)
	     (substring attribute  (match-end 0))
	   attribute)))

    (loop for x in (dom-node-attributes node)
	  when (string= (dom-attr-name x) attribute-name)
	  return (dom-attr-value x))
    )
  )
; (xpath-attribute  svn-entry "path")
; (xpath-attribute  svn-entry "notexist")


(provide 'xpath-helpers)
