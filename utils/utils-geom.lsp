;;; <LISPDOC>
;;; <SUBR>(geom-point-in-bbox pt ptlu ptrd)</SUBR>
;;; <DESC>Detect point in bounding box</DESC>
;;; <ARG>pt - point to test</ARG>
;;; <ARG>ptlu - upper left corner point</ARG>
;;; <ARG>ptrd - lower right point</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun geom-point-in-bbox (pt ptlu ptrd /)
  (if
    (and
      (>= (car pt) (car ptlu))
      (<= (car pt) (car ptrd))
      (>= (cadr pt) (cadr ptrd))
      (<= (cadr pt) (cadr ptlu)))
    T
    nil))