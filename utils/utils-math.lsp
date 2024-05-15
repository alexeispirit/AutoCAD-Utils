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

;;; <LISPDOC>
;;; <SUBR>(math-avg lst)</SUBR>
;;; <DESC>Calculate average value</DESC>
;;; <ARG>lst - list of numbers</ARG>
;;; <RET>Average value</RET>
;;; </LISPDOC>
(defun math-avg (lst)
  (if lst
    (/ (apply '+ lst) (length lst))))

;;; <LISPDOC>
;;; <SUBR>(math-tan x)</SUBR>
;;; <DESC>Calculate tangent</DESC>
;;; <ARG>x - angle in radians</ARG>
;;; <RET>Tangent of x</RET>
;;; </LISPDOC>
(defun math-tan (x)
  (if (not (equal 0.0 (cos x) 1e-10))
    (/ (sin x) (cos x))))

;;; <LISPDOC>
;;; <SUBR>(math-cot x)</SUBR>
;;; <DESC>Calculate cotangent</DESC>
;;; <ARG>x - angle in radians</ARG>
;;; <RET>Cotangent of x</RET>
;;; </LISPDOC>
(defun math-cot (x)
  (if (not (equal 0.0 (sin x) 1e-10))
    (/ (cos x) (sin x))))

;;; <LISPDOC>
;;; <SUBR>(math-asin x)</SUBR>
;;; <DESC>Calculate arcsine</DESC>
;;; <ARG>x - value</ARG>
;;; <RET>Arcsine of x</RET>
;;; </LISPDOC>
(defun math-asin (x)
  (if (<= -1.0 x 1.0)
    (atan x (sqrt (- 1.0 (* x x))))))

;;; <LISPDOC>
;;; <SUBR>(math-acos x)</SUBR>
;;; <DESC>Calculate arccosine</DESC>
;;; <ARG>x - value</ARG>
;;; <RET>Arccosine of x</RET>
;;; </LISPDOC>
(defun math-acos (x)
  (if (<= -1.0 x 1.0)
    (atan (sqrt (- 1.0 (* x x))) x)))



