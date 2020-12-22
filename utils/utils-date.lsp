;;; <LISPDOC>
;;; <SUBR>(date-parse-string str)</SUBR>
;;; <DESC>Parse date in "dd.mm.yyyy" format to list ("DD" "MM" "YYYY")</DESC>
;;; <ARG>str - date in "DD.MM.YYYY" format</ARG>
;;; <RET>list with date values</RET>
;;; </LISPDOC>
(defun date-parse-string (str / lst) 
  (if (>= 10 (strlen str) 8) 
    (setq lst (string-split-to-list 
                str
                (car (regexp-execute-to-plain-list (regexp-regapp) "\\W+" str)))))
  (if (= (length lst) 3) 
    lst))

;;; <LISPDOC>
;;; <SUBR>(date-format-string-gost datelst)</SUBR>
;;; <DESC>Format date to string in "DD.MM.YY" format</DESC>
;;; <ARG>datelst - date in ("DD" "MM" "YYYY") format</ARG>
;;; <RET>formatted string</RET>
;;; </LISPDOC>
(defun date-format-string-gost (datelst / lastv) 
  (if datelst 
    (list-join-to-string 
      (list 
        (car datelst)
        (cadr datelst)
        (if (= (strlen (setq lastv (last datelst))) 4) 
          (substr lastv 3 4)
          lastv))
      ".")))