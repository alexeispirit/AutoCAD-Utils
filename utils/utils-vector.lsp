;;; <LISPDOC>
;;; <SUBR>(vector-multiply-number vec k)</SUBR>
;;; <DESC>Multiplies vector by given number</DESC>
;;; <ARG>vec - vector</ARG>
;;; <ARG>k - number</ARG>
;;; <RET>Result of vector product by number</RET>
;;; </LISPDOC>  
(defun vector-multiply-number (vec k)
  (mapcar '(lambda (x) (* x k)) vec))

;;; <LISPDOC>
;;; <SUBR>(vector-scalar-product vec1 vec2)</SUBR>
;;; <DESC>Vector scalar product</DESC>
;;; <ARG>vec1 - vector 1</ARG>
;;; <ARG>vec2 - vector 2</ARG>
;;; <RET>Result of vector scalar product</RET>
;;; </LISPDOC>
(defun vector-scalar-product (vec1 vec2)
  (apply '+ (mapcar '* vec1 vec2)))

;;; <LISPDOC>
;;; <SUBR>(vector-cross-product vec1 vec2)</SUBR>
;;; <DESC>Vector cross product</DESC>
;;; <ARG>vec1 - vector 1</ARG>
;;; <ARG>vec2 - vector 2</ARG>
;;; <RET>Result of vector cross product</RET>
;;; </LISPDOC>
(defun vector-cross-product (vec1 vec2)
  (list
    (- (* (cadr vec1) (caddr vec2)) (* (cadr vec2) (caddr vec1)))
    (- (* (car vec2) (caddr vec1)) (* (car vec1) (caddr vec2)))
    (- (* (car vec1) (cadr vec2)) (* (car vec2) (cadr vec1)))))

;;; <LISPDOC>
;;; <SUBR>(vector-length vec)</SUBR>
;;; <DESC>Calculates vector length</DESC>
;;; <ARG>vec - vector</ARG>
;;; <RET>Vector length</RET>
;;; </LISPDOC>
(defun vector-length (vec)
  (sqrt (apply '+ (mapcar '* vec vec))))

;;; <LISPDOC>
;;; <SUBR>(vector-unit vec)</SUBR>
;;; <DESC>Calculates unit vector</DESC>
;;; <ARG>vec - vector</ARG>
;;; <RET>Unit vector</RET>
;;; </LISPDOC>
(defun vector-unit (vec)
  (vector-multiply-number vec (/ 1.0 (vector-length vec))))