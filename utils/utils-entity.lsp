;;; <LISPDOC>
;;; <SUBR>(entity-dxf-get code entity)</SUBR>
;;; <DESC>Get provided dxf code information from entity</DESC>
;;; <ARG>code - dxf code to retrieve</ARG>
;;; <ARG>entity - entity (ENAME) to query</ARG>
;;; <RET>code value</RET>
;;; </LISPDOC>
(defun entity-dxf-get (code entity)
  (cdr (assoc code (entget entity))))

;;; <LISPDOC>
;;; <SUBR>(entity-dxf-set code entity value)</SUBR>
;;; <DESC>Set provided dxf code value for entity</DESC>
;;; <ARG>code - dxf code to set up</ARG>
;;; <ARG>entity - entity (ENAME) to query</ARG>
;;; <ARG>value - value to set (depends on dxf code)</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun entity-dxf-set (code entity value / entdef)
  (setq entdef (entget entity)
	entdef (if (assoc code entdef)
		 (subst (cons code value) (assoc code entdef) entdef)
		 (append entdef (list (cons code value)))))
  (entmod entdef)
  (entupd entity))