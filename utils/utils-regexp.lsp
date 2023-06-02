;;; <LISPDOC>
;;; <SUBR>(regexp-regapp)</SUBR>
;;; <DESC>Get VBScript.RegExp pointer</DESC>
;;; <RET>VBScript.RegExp pointer</RET>
;;; </LISPDOC>
(defun regexp-regapp () 
  (vla-getinterfaceobject (vlax-get-acad-object) "VBScript.RegExp")
)

;;; <LISPDOC>
;;; <SUBR>(regexp-match regexp_object pattern test_string is_global case_sensitive)</SUBR>
;;; <DESC>Test string match with Regexp</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <ARG>pattern - regexp pattern</ARG>
;;; <ARG>test_string - string to test</ARG>
;;; <ARG>is_global - global match key (g)</ARG>
;;; <ARG>case_sensitive - case sensitive key (i)</ARG>
;;; <RET>T if matches \ nil otherwise</RET>
;;; </LISPDOC>
(defun regexp-match (regexp_object pattern test_string is_global case_sensitive / 
                     result
                    ) 
  (if regexp_object 
    (progn 
      (vlax-put regexp_object 'Pattern pattern)
      (vlax-put regexp_object 'Global (if is_global acTrue acFalse))
      (vlax-put regexp_object 'IgnoreCase (if case_sensitive acFalse acTrue))
      (setq result (vlax-invoke regexp_object 'Test test_string))
      (vlax-put regexp_object 'Pattern "")
    )
  )
  (if (and result (/= result 0)) 
    T
    nil
  )
)

;;; <LISPDOC>
;;; <SUBR>(regexp-replace regexp_object pattern replace_string test_string is_global case_sensitive)</SUBR>
;;; <DESC>Replace Regexp pattern with string</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <ARG>pattern - regexp pattern</ARG>
;;; <ARG>replace_string - string replacement</ARG>
;;; <ARG>test_string - string to test</ARG>
;;; <ARG>is_global - global match key (g)</ARG>
;;; <ARG>case_sensitive - case sensitive key (i)</ARG>
;;; <RET>String after replace</RET>
;;; </LISPDOC>
(defun regexp-replace (regexp_object pattern replace_string test_string is_global 
                       case_sensitive / result
                      ) 
  (if regexp_object 
    (progn 
      (vlax-put regexp_object 'Pattern pattern)
      (vlax-put regexp_object 'Global (if is_global acTrue acFalse))
      (vlax-put regexp_object 'IgnoreCase (if case_sensitive acFalse acTrue))
      (setq result (vlax-invoke regexp_object 'Replace test_string replace_string))
      (vlax-put regexp_object 'Pattern "")
    )
  )
  result
)

;;; <LISPDOC>
;;; <SUBR>(regexp-execute regexp_object pattern test_string is_global case_sensitive)</SUBR>
;;; <DESC>Execute regexp and return collection of found strings</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <ARG>pattern - regexp pattern</ARG>
;;; <ARG>test_string - string to test</ARG>
;;; <ARG>is_global - global match key (g)</ARG>
;;; <ARG>case_sensitive - case sensitive key (i)</ARG>
;;; <RET>List of found strings ((String Index Length)...)</RET>
;;; </LISPDOC>
(defun regexp-execute (regexp_object pattern test_string is_global case_sensitive / 
                       result subresult collection
                      ) 
  (if regexp_object 
    (progn 
      (vlax-put regexp_object 'Pattern pattern)
      (vlax-put regexp_object 'Global (if is_global acTrue acFalse))
      (vlax-put regexp_object 'IgnoreCase (if case_sensitive acFalse acTrue))
      (setq collection (vlax-invoke regexp_object 'Execute test_string))
      (vlax-put regexp_object 'Pattern "")
    )
  )
  (vlax-for item 
            collection
            (setq result (cons 
                           (list 
                             (vlax-get item 'Value)
                             (vlax-get item 'FirstIndex)
                             (vlax-get item 'Length)
							 (vlax-for subitem (vlax-get item 'Submatches)
								(setq subresult (cons subitem subresult)))
                           )
                           result
                         )
            )
  )
)

;;; <LISPDOC>
;;; <SUBR>(regexp-execute-to-plain-list regexp_object regexp string)</SUBR>
;;; <DESC>Search for regexp in string</DESC>
;;; <ARG>regexp_object - VBScript.RegExp</ARG>
;;; <ARG>regexp - regexp to test</ARG>
;;; <ARG>string - string to test</ARG>
;;; <RET>list of found strings</RET>
;;; </LISPDOC>
(defun regexp-execute-to-plain-list (regexp_object regexp string / strlist) 
  (foreach item (regexp-execute regexp_object regexp string T nil) 
    (setq strlist (append (list (car item)) strlist))
  )
  strlist
)