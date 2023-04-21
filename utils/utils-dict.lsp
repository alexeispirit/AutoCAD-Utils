;;; <LISPDOC>
;;; <SUBR>(dict-get-collection)</SUBR>
;;; <DESC>Service subroutine to get document dictionaries collection</DESC>
;;; <RET>VLA-collection</RET>
;;; </LISPDOC>
(defun dict-get-collection ( / )
  (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))))

;;; <LISPDOC>
;;; <SUBR>(dict-get-all)</SUBR>
;;; <DESC>Print all dictionary names to command line</DESC>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun dict-get-all ( / item)
  (vlax-for item (dict-get-collection)
    (if (vlax-property-available-p item 'Name)
      (editor-say (vlax-get item 'Name)))))

;;; <LISPDOC>
;;; <SUBR>(dict-remove-by-mask)</SUBR>
;;; <DESC>Remove dictionaries in document by mask</DESC>
;;; <ARG>mask - wcmatch string</ARG>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun dict-remove-by-mask (mask / item)
  (vlax-for item (dict-get-collection)
    (if (vlax-property-available-p item 'Name)
      (if (wcmatch (vlax-get item 'Name) mask)
        (vla-delete item)))))