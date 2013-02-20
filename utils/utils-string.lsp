(vl-load-com) ; remove after main

;;; <LISPDOC>
;;; <SUBR>(string-remove-pattern regexp_object pattern str)</SUBR>
;;; <DESC>Remove pattern from string</DESC>
;;; <ARG>pattern - pattern to remove</ARG>
;;; <ARG>str - string to clear</ARG>
;;; <RET>cleared string</RET>
;;; </LISPDOC>
(defun string-remove-pattern (regexp_object pattern str)
  (regexp-replace regexp_object pattern "" str T nil))

;;; <LISPDOC>
;;; <SUBR>(string-format-list)</SUBR>
;;; <DESC>List of MText format symbols</DESC>
;;; <RET>list of MText string format symbols</RET>
;;; </LISPDOC>
(defun string-format-list ()
  (list
    (list "Alignment" 	"\\\\A[012];")
    (list "Tabs"	"\\t")
    (list "Color"	"\\\\[Cc][0-9]?[.]?[0-9]+;")
    (list "Font"	"\\\\[Ff].*?;")
    (list "Height"	"\\\\H[0-9]?[.]?[0-9]+x;")
    (list "Linefeed"	"^\\P| \\\\P|\\\\P |\\\\P")
    (list "Slashl"	"\\\\l")
    (list "Overline"	"\\\\[Oo]")
    (list "Paragraph"	"\\\\p.*?;")
    (list "Oblique"	"\\\\Q[-]?[0-9]*?[.]?[0-9]+;")
    (list "Stacking"	"\\\\S.*?\\;")
    (list "Tracking"	"\\\\T[0-9]?[.]?[0-9]+;")
    (list "Width"	"\\\\W[0-9]?[.]?[0-9]+;")
    (list "Braces"	"[{}]")))

;;; <LISPDOC>
;;; <SUBR>(string-remove-format regexp str)</SUBR>
;;; <DESC>Remove all format characters from MText string</DESC>
;;; <ARG>regexp_object - VBScript.RegExp pointer</ARG>
;;; <ARG>str - MText string to clear</ARG>
;;; <RET>cleared string</RET>
;;; </LISPDOC>
(defun string-remove-format (regexp_object str / )
  (foreach item (string-format-list)
    (setq str (string-remove-pattern regexp_object (cadr item) str)))
  str)

;;; <LISPDOC>
;;; <SUBR>(string-is-null-or-empty str)</SUBR>
;;; <DESC>Check whether string is empty or nil</DESC>
;;; <ARG>str - string to test</ARG>
;;; <RET>T - if empty \ nil - otherwise</RET>
;;; </LISPDOC>
(defun string-is-null-or-empty (str / )
  (if (or (not str) (= str "") (= str " "))
    T
    nil))

;;; <LISPDOC>
;;; <SUBR>(string-is-a-comment str)</SUBR>
;;; <DESC>Check whether string is a comment (;)</DESC>
;;; <ARG>str - string to test</ARG>
;;; <RET>T - if comment \ nil - otherwise</RET>
;;; </LISPDOC>
(defun string-is-a-comment (str / )
  (if (= ";" (substr str 1 1))
    T
    nil))

;;; <LISPDOC>
;;; <SUBR>(string-split-to-list str splitter)</SUBR>
;;; <DESC>Split string into list by pattern</DESC>
;;; <ARG>str - string to split</ARG>
;;; <ARG>splitter - delimiter</ARG>
;;; <RET>list of splitted strings</RET>
;;; </LISPDOC>
(defun string-split-to-list (str splitter / i)
  (cond
    ((= str "") nil)
    ((setq i (vl-string-search splitter str))
     (cons (substr str 1 i)
	   (string-split-to-list (substr str (+ (strlen splitter) 1 i)) splitter)))
    (t (list str))))

;;; <LISPDOC>
;;; <SUBR>(string-make-pair str)</SUBR>
;;; <DESC>Split string by space and check if it has 2 elements \
;;; ("GOST 21.1001-2009") -> ("GOST" "21.1001-2009")</DESC>
;;; <ARG>str - string to split</ARG>
;;; <RET>list of splitted strings if list has 2 elements \
;;; nil otherwise</RET>
;;; </LISPDOC>
(defun string-make-pair (str / pair number index)
  (setq pair (string-split-to-list str " ")
	number (car (reverse pair))
	index (list-join-to-string (reverse (cdr (reverse pair))) " ")
	pair (list index number))  
  (if (= 2 (length pair))
    pair
    nil))

;;; <LISPDOC>
;;; <SUBR>(string-remove-with-whitelist-rule str rule)</SUBR>
;;; <DESC>Remove trash from string with whitelist rule</DESC>
;;; <ARG>str - string to clear</ARG>
;;; <ARG>rule - rule from whitelist to apply</ARG>
;;; <RET>cleared string</RET>
;;; </LISPDOC>
(defun string-remove-with-whitelist-rule (str rule / i)
  (if (setq i (vl-string-search rule str))
    (substr str (1+ i) (strlen str))
    str))


;;; <LISPDOC>
;;; <SUBR>(string-remove-with-whitelist-total str whitelist)</SUBR>
;;; <DESC>Remove trash from string with whitelist</DESC>
;;; <ARG>str - string to clear</ARG>
;;; <ARG>whitelist - whitelist to apply</ARG>
;;; <RET>cleared string</RET>
;;; </LISPDOC>
(defun string-remove-with-whitelist-total (str whitelist)
  (if whitelist
    (foreach rule whitelist
      (setq str (string-remove-with-whitelist-rule str rule))))
  str)

;;; <LISPDOC>
;;; <SUBR>(string-search-reverse item str)</SUBR>
;;; <DESC>Reverse search in string</DESC>
;;; <ARG>item - item to look for</ARG>
;;; <ARG>str - string to search in</ARG>
;;; <RET>item index or nil</RET>
;;; </LISPDOC>
(defun string-search-reverse (item str / )
  (vl-string-position (ascii item) str 0 T))

;;; <LISPDOC>
;;; <SUBR>(string-search item str)</SUBR>
;;; <DESC>search in string, alias to vl-string-search</DESC>
;;; <ARG>item - item to look for</ARG>
;;; <ARG>str - string to search in</ARG>
;;; <RET>item index or nil</RET>
;;; </LISPDOC>
(defun string-search (item str)
  (vl-string-search item str))

;;; <LISPDOC>
;;; <SUBR>(string-contains str template)</SUBR>
;;; <DESC>Check if string contains template</DESC>
;;; <ARG>str - string to search in</ARG>
;;; <ARG>template - template to search</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun string-contains (str template)
  (if (and
	(= (type str) 'STR)
	(= (type template) 'STR))
    (wcmatch str (strcat "*" template "*"))))  
  
;;; <LISPDOC>
;;; <SUBR>(string-regexp-replace-fast pattern replacer str)</SUBR>
;;; <DESC>Replace regexp pattern with string. Autoregister application</DESC>
;;; <ARG>pattern - regexp pattern</ARG>
;;; <ARG>replacer - string replacement</ARG>
;;; <ARG>str - string to search in</ARG>
;;; <RET>new string</RET>
;;; </LISPDOC>
(defun string-regexp-replace-fast (pattern replacer str)
  (regexp-replace (regexp-regapp) pattern replacer str T nil))

;;; <LISPDOC>
;;; <SUBR>(string-trim-symbols pattern str)</SUBR>
;;; <DESC>Trim symbols from string. Autoregister application</DESC>
;;; <ARG>pattern - regexp pattern</ARG>
;;; <ARG>str - string to search in</ARG>
;;; <RET>new string</RET>
;;; </LISPDOC>
(defun string-trim-symbols (pattern str)
  (if str
    (regexp-replace (regexp-regapp) (strcat "^" pattern) ""
      (regexp-replace (regexp-regapp) (strcat pattern "$") "" str T nil)
      T nil)))
    