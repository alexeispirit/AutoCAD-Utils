;;; <LISPDOC>
;;; <SUBR>(editor-say what)</SUBR>
;;; <DESC>Prints in editor any value with "\n"</DESC>
;;; <ARG>what - value to print</ARG>
;;; <RET>Nothing</RET>
;;; </LISPDOC>
(defun editor-say (what) 
  (princ (strcat (vl-princ-to-string what) "\n"))
  (princ))