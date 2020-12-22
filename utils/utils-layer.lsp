;;; <LISPDOC>
;;; <SUBR>(layer-remove-references)</SUBR>
;;; <DESC>Remove all reconciled layers references</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun layer-remove-references (/ layer xrec xlist item) 
  (setq layer (tblnext "LAYER" T))
  (while layer 
    (setq layer (entget (tblobjname "LAYER" (cdr (assoc 2 layer)))))
    (while (setq xrec (assoc 360 layer)) 
      (if (= (cdr (assoc 3 (entget xrec))) "ADSK_XREC_LAYER_RECONCILED") 
        (progn 
          (setq xlist (append xlist (list (cdr xrec))))
          (setq layer (vl-remove xrec layer))
          (entmod layer)))
      (setq layer (tblnext "LAYER")))
    (foreach item (list-remove-duplicates xlist) 
      (entdel item)))
  nil)