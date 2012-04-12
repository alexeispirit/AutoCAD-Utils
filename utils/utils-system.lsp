;;; <LISPDOC>
;;; <SUBR>(system-computer-name)</SUBR>
;;; <DESC>Reveal computer name</DESC>
;;; <RET>computername string</RET>
;;; </LISPDOC>
(defun system-computer-name ()
  (getenv "COMPUTERNAME"))

;;; <LISPDOC>
;;; <SUBR>(system-user-name)</SUBR>
;;; <DESC>Reveal user name</DESC>
;;; <RET>username string</RET>
;;; </LISPDOC>
(defun system-user-name ()
  (getenv "USERNAME"))

;;; <LISPDOC>
;;; <SUBR>(system-computer-stp-name pc)</SUBR>
;;; <DESC>Convert computername into canonical stpname<DESC>
;;; <ARG>pc - computername string</ARG>
;;; <RET>canonical stp computername</RET>
;;; </LISPDOC>
(defun system-computer-stp-name (pc /)
  (string-regexp-replace-fast "\\d+" "" (strcase (substr pc 1 (string-search-reverse "-" pc)))))