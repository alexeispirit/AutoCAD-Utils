;;; <LISPDOC>
;;; <SUBR>(math-degrees-radians angle)</SUBR>
;;; <DESC>Convert degrees into radians</DESC>
;;; <ARG>angle - angle in degrees</ARG>
;;; <RET>Angle in radians</RET>
;;; </LISPDOC>
(defun math-degrees-radians (angle) 
  (* pi (/ angle 180.0)))

;;; <LISPDOC>
;;; <SUBR>(math-radians-degrees angle)</SUBR>
;;; <DESC>Convert radians into degrees</DESC>
;;; <ARG>angle - angle in radians</ARG>
;;; <RET>Angle in degrees</RET>
;;; </LISPDOC>
(defun math-radians-degrees (angle) 
  (* 180.0 (/ angle pi)))
