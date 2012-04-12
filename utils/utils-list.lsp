;;; <LISPDOC>
;;; <SUBR>(list-join-to-string strlist splitter)</SUBR>
;;; <DESC>Join list of strings into string</DESC>
;;; <ARG>strlist - list of strings</ARG>
;;; <ARG>splitter - delimiter</ARG>
;;; <RET>Joined string</RET>
;;; </LISPDOC>
(defun list-join-to-string (strlist splitter)
  (if strlist
    (strcat
      (car strlist)
      (if (< 0 (length (cdr strlist)))
	splitter
	"")
      (list-join-to-string (cdr strlist) splitter))
    ""))

;;; <LISPDOC>
;;; <SUBR>(list-remove-duplicates lst)</SUBR>
;;; <DESC>Remove duplicates from list</DESC>
;;; <ARG>lst - list to proceed</ARG>
;;; <RET>cleared list</RET>
;;; </LISPDOC>
(defun list-remove-duplicates (lst)
  (if lst
    (cons (car lst) (list-remove-duplicates (vl-remove (car lst) lst)))))

;;; <LISPDOC>
;;; <SUBR>(list-union list1 list2)</SUBR>
;;; <DESC>Union list function</DESC>
;;; <ARG>list1, list2 - lists to union</ARG>
;;; <RET>united list</RET>
;;; </LISPDOC>
(defun list-union (list1 list2 / outlist)
  (setq outlist list1)
    (foreach item2 list2
      (if (not (vl-position item2 outlist))
	(setq outlist (append outlist (list item2)))))
  (vl-sort outlist '<))

;;; <LISPDOC>
;;; <SUBR>(list-intersect list1 list2)</SUBR>
;;; <DESC>Intersect list function</DESC>
;;; <ARG>list1, list2 - lists to intersect</ARG>
;;; <RET>intersected list</RET>
;;; </LISPDOC>
(defun list-intersect (list1 list2 / outlist)
  (setq outlist (list ))
  (foreach item2 list2
    (if (and (vl-position item2 list1) (not (vl-position item2 outlist)))
      (setq outlist (append outlist (list item2)))))
  (vl-sort outlist '<))

;;; <LISPDOC>
;;; <SUBR>(list-substract list1 list2)</SUBR>
;;; <DESC>Substract list function</DESC>
;;; <ARG>list1, list2 - lists to substract</ARG>
;;; <RET>substracted list</RET>
;;; </LISPDOC>
(defun list-substract (list1 list2 / outlist)
  (setq outlist list1)
  (foreach item2 list2
    (if (vl-position item2 list1)
      (setq outlist (vl-remove item2 outlist))
      (setq outlist (append outlist (list item2)))))
  (vl-sort outlist '<))

;;; <LISPDOC>
;;; <SUBR>(list-xrange xfirst xlast xstep)</SUBR>
;;; <DESC>Make range of num values</DESC>
;;; <ARG>xfirst - first range value</ARG>
;;; <ARG>xlast - last range value (excluded)</ARG>
;;; <ARG>xstep - range step</ARG>
;;; <RET>Range list</RET>
;;; </LISPDOC>
(defun list-xrange (xfirst xlast xstep)
  (if (< xfirst xlast)
    (cons xfirst (list-xrange (+ xfirst xstep) xlast xstep))))