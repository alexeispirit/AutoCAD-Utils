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
      (vl-princ-to-string (car strlist))
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
;;; <

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

;;; <LISPDOC>
;;; <SUBR>(list-cdrr lst)</SUBR>
;;; <DESC>Acts like (cdr) but removes all repeating members from beginning</DESC>
;;; <ARG>lst - list to cdrr</ARG>
;;; <RET>list without first n elements</RET>
;;; </LISPDOC>
(defun list-cdrr (lst / )
  (if lst
    (cond
      ((= (car lst) (cadr lst))
	  (list-cdrr (cdr lst)))
      (t (cdr lst)))))

;;; <LISPDOC>
;;; <SUBR>(list-remove-repeats lst)</SUBR>
;;; <DESC>Removes repeating in a row members of the list</DESC>
;;; <ARG>lst - lst to remove items</ARG>
;;; <RET>lst without repeating members</RET>
;;; </LISPDOC>
(defun list-remove-repeats (lst /)
  (if lst
    (cons
      (car lst)
      (list-remove-repeats (list-cdrr lst)))))

;;; <LISPDOC>
;;; <SUBR>(list-cdrr-prs lst prs)</SUBR>
;;; <DESC>Acts like (cdr) but removes all repeating members from beginning with precision</DESC>
;;; <ARG>lst - list to cdrr</ARG>
;;; <ARG>prs - presision</ARG>
;;; <RET>list without first n elements</RET>
;;; </LISPDOC>
(defun list-cdrr-prs (lst prs / )
  (if lst
    (cond
      ((equal (car lst) (cadr lst) prs)
	  (list-cdrr-prs (cdr lst) prs))
      (t (cdr lst)))))

;;; <LISPDOC>
;;; <SUBR>(list-remove-repeats-prs lst prs)</SUBR>
;;; <DESC>Removes repeating in a row members of the list with precision</DESC>
;;; <ARG>lst - lst to remove items</ARG>
;;; <ARG>prs - presision</ARG>
;;; <RET>lst without repeating members</RET>
;;; </LISPDOC>
(defun list-remove-repeats-prs (lst prs /)
  (if lst
    (cons
      (car lst)
      (list-remove-repeats-prs (list-cdrr-prs lst prs) prs))))


;;; <LISPDOC>
;;; <SUBR>(list-massoc key lst)</SUBR>
;;; <DESC>Multiple list assoc</DESC>
;;; <ARG>key - key to assoc</ARG>
;;; <ARG>lst - lst to assoc key</ARG>
;;; <RET>list of found items</RET>
;;; </LISPDOC>
(defun list-massoc (key lst)
  (if lst
    (mapcar 'cdr (vl-remove-if-not '(lambda (x) (eq key (car x))) lst))))
  