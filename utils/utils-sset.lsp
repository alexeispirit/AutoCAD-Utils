;;; <LISPDOC>
;;; <SUBR>(sset-join)</SUBR>
;;; <DESC>Join 2 selection sets</DESC>
;;; <ARG>ss1 - Selection set</ARG>
;;; <ARG>ss2 - Selection set</ARG>
;;; <RET>New Selection set</RET>
;;; </LISPDOC>  
(defun sset-join (ss1 ss2) 
  (repeat (setq k (sslength ss2)) 
    (setq k (1- k))
    (ssadd (ssname ss2 k) ss1)
  )
  ss1
)

;;; <LISPDOC>
;;; <SUBR>(sset-all-database entity-filter)</SUBR>
;;; <DESC>Select all entities in database</DESC>
;;; <ARG>entity-filter - string for entity filter. "*" for all</ARG>
;;; <RET>New Selection set</RET>
;;; </LISPDOC>  
(defun sset-all-database (entity-filter) 
  (ssget "_X" (list (cons 0 entity-filter)))
)

;;; <LISPDOC>
;;; <SUBR>(sset-all-current-space entity-filter)</SUBR>
;;; <DESC>Select all entities in current space</DESC>
;;; <ARG>entity-filter - string for entity filter. "*" for all</ARG>
;;; <RET>New Selection set</RET>
;;; </LISPDOC>  
(defun sset-all-current-space (entity-filter) 
  (ssget "_X" 
         (list 
           (cons 0 entity-filter)
           (cons 67 (- 1 (getvar "TILEMODE")))
           (cons 410 (acad-actlayout-name nil))
         )
  )
)

;;; <LISPDOC>
;;; <SUBR>(sset-all-by-layer entity-filter layer)</SUBR>
;;; <DESC>Select all entities in current space and layer name</DESC>
;;; <ARG>entity-filter - string for entity filter. "*" for all</ARG>
;;; <ARG>layer - string for layer name</ARG>
;;; <RET>New Selection set</RET>
;;; </LISPDOC>
(defun sset-all-by-layer (entity-filter layer) 
  (ssget "_X" 
         (list 
           (cons 0 entity-filter)
           (cons 67 (- 1 (getvar "TILEMODE")))
           (cons 410 (acad-actlayout-name nil))
           (cons 8 layer)
         )
  )
)

;;; <LISPDOC>
;;; <SUBR>(sset-by-user entity-filter)</SUBR>
;;; <DESC>Select entities by user</DESC>
;;; <ARG>entity-filter - string for entity filter. "*" for all</ARG>
;;; <RET>New Selection set</RET>
;;; </LISPDOC>
(defun sset-by-user (entity-filter) 
  (ssget (list (cons 0 entity-filter)))
)