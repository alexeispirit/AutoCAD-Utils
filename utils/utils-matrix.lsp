;;; <LISPDOC>
;;; <SUBR>(matrix-remove-rowcol mat size row col)</SUBR>
;;; <DESC>Removes given row and column from given matrix</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <ARG>row - index of row to remove</ARG>
;;; <ARG>col - index of column to remove</ARG>
;;; <RET>New matrix without give row and column</RET>
;;; </LISPDOC>
(defun matrix-remove-rowcol (mat size row col / newrow newmat currow i j)
  (setq newmat nil)
  (setq i 0)
  (repeat size
    (if (not (= i row))
      (progn
        (setq newrow nil j 0)
        (setq currow (nth i mat))
        (repeat size
          (if (not (= j col))
            (setq newrow (append newrow (list (nth j currow)))))
          (setq j (1+ j)))
        (setq newmat (append newmat (list newrow)))))
    (setq i (1+ i)))
  newmat)
  
;;; <LISPDOC>
;;; <SUBR>(matrix-determinant mat size)</SUBR>
;;; <DESC>Calculates determinant of square matrix</DESC>
;;; <ARG>mat - square matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <RET>Matrix determinant</RET>
;;; </LISPDOC>  
(defun matrix-determinant (mat size / det degree newmat j)
; https://mindhalls.ru/matrix-determinant-calculation-recursive/
  (setq det 0)
  (setq degree 1)
  
  (cond 
    ((= size 1)
      (caar mat))
    ((= size 2)
      (- (* (caar mat) (cadadr mat)) (* (cadar mat) (caadr mat))))
    ((> size 2)
      (setq j 0)
      (repeat size
        (setq newmat (matrix-remove-rowcol mat size 0 j))
        (setq det (+ det (* degree (nth j (car mat)) (matrix-determinant newmat (1- size)))))
        (setq degree (* -1 degree))
        (setq j (1+ j)))
      det)))

;;; <LISPDOC>
;;; <SUBR>(matrix-replace-col mat size col values)</SUBR>
;;; <DESC>Replaces column of matrix with given values</DESC>
;;; <ARG>mat - square matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <ARG>col - index of column</ARG>
;;; <ARG>values - list of values</ARG>
;;; <RET>Matrix with replaced column</RET>
;;; </LISPDOC>      
(defun matrix-replace-col (mat size col values / newmat newrow currow i j)
  (if (= (length mat) (length values))
    (progn
      (setq newmat nil)
      (setq i 0)
      (repeat size
        (setq newrow nil j 0)
        (setq currow (nth i mat))
        (repeat size
          (setq colvalue (if (= j col) (nth j values) (nth j currow)))
          (setq newrow (append newrow (list colvalue)))
          (setq j (1+ j)))
        (setq newmat (append newmat (list newrow)))
        (setq i (1+ i)))
    newmat)))

;;; <LISPDOC>
;;; <SUBR>(matrix-from-list lst size)</SUBR>
;;; <DESC>Generates square matrix from given list</DESC>
;;; <ARG>lst - list of values</ARG>
;;; <ARG>size - size of new matrix</ARG>
;;; <RET>Matrix</RET>
;;; </LISPDOC>      
(defun matrix-from-list (lst size / i newlst tmplst)
  (while lst
    (setq tmplst nil i 0)
    (while (< i size)
      (setq tmplst (append tmplst (list (car lst)))
            lst (cdr lst)
            i (1+ i)))
    (setq newlst (append newlst (list tmplst))))
  newlst)

;;; <LISPDOC>
;;; <SUBR>(matrix-to-list mat)</SUBR>
;;; <DESC>Generate square matrix from given list</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <RET>Plain list</RET>
;;; </LISPDOC>      
(defun matrix-to-list (mat)
  (apply 'append mat))

;;; <LISPDOC>
;;; <SUBR>(matrix-multiply-matrix a b)</SUBR>
;;; <DESC>Performs matrix on matrix multiplication</DESC>
;;; <ARG>a - matrix 1</ARG>
;;; <ARG>b - matrix 2</ARG>
;;; <RET>Result of matrix multiplication</RET>
;;; </LISPDOC>      
(defun matrix-multiply-matrix (a b / c i j k sum)
  (setq i 0)
  (repeat (length a) ;i
    (setq j 0)
    (repeat (length (car b)) ;j
      (setq sum 0 k 0)
      (repeat (length b) ;k
        (setq sum (+ sum (* (nth k (nth i a)) (nth j (nth k b)))))
        (setq k (1+ k)))
      (setq c (append c (list sum)))
      (setq j (1+ j)))
    (setq i (1+ i)))    
  (matrix-from-list c (length a)))


;;; <LISPDOC>
;;; <SUBR>(matrix-multiply-scalar mat k)</SUBR>
;;; <DESC>Performs matrix on scalar multiplication</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <ARG>k - number</ARG>
;;; <RET>Result of matrix multiplication</RET>
;;; </LISPDOC>      
(defun matrix-multiply-scalar (mat k / i j)
  (setq newmat nil)
  (setq i 0)
  (repeat (length mat)
    (setq newrow nil j 0)
    (setq currow (nth i mat))
    (repeat (length currow)
      (setq colvalue (* k (nth j currow)))
      (setq newrow (append newrow (list colvalue)))
      (setq j (1+ j)))
    (setq newmat (append newmat (list newrow)))
    (setq i (1+ i)))
  newmat)

;;; <LISPDOC>
;;; <SUBR>(matrix-transpose mat)</SUBR>
;;; <DESC>Transposes matrix</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <RET>Transposed matrix</RET>
;;; </LISPDOC>     
(defun matrix-transpose (mat / newmat k)
  (setq newmat nil k 0)
  (repeat (length (car mat))
    (setq newmat (append newmat (list (mapcar (function (lambda (x) (nth k x))) mat))))
    (setq k (1+ k)))
  newmat)

;;; <LISPDOC>
;;; <SUBR>(matrix-complement mat size row col)</SUBR>
;;; <DESC>Calculates matrix complement for given element</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <ARG>row - index of row</ARG>
;;; <ARG>col - index of column</ARG>
;;; <RET>Matrix complement for element</RET>
;;; </LISPDOC>
(defun matrix-complement (mat size row col / )
  (* (expt -1 (+ row col)) (matrix-determinant (matrix-remove-rowcol mat size row col) (1- size))))

;;; <LISPDOC>
;;; <SUBR>(matrix-union mat size)</SUBR>
;;; <DESC>Calculates union matrix</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <RET>Union matrix</RET>
;;; </LISPDOC>
(defun matrix-union (mat size / newmat newrow currow i j)
  (setq newmat nil i 0)
  (repeat size
    (setq newrow nil j 0)
    (setq currow (nth i mat))
    (repeat size
      (setq colvalue (matrix-complement mat size i j))
      (setq newrow (append newrow (list colvalue)))
      (setq j (1+ j)))
    (setq newmat (append newmat (list newrow)))
    (setq i (1+ i)))
  newmat)

;;; <LISPDOC>
;;; <SUBR>(matrix-inverse mat size)</SUBR>
;;; <DESC>Calculates inverse matrix</DESC>
;;; <ARG>mat - matrix</ARG>
;;; <ARG>size - size of matrix</ARG>
;;; <RET>Inversed matrix</RET>
;;; </LISPDOC>    
(defun matrix-inverse (mat size / det)
  (setq det (matrix-determinant mat size))
  (if (/= det 0)
    (matrix-multiply-scalar (matrix-transpose (matrix-union mat size)) (/ 1.0 det))))