;;; <LISPDOC>
;;; <SUBR>(block-ref-safe-reset ent)</SUBR>
;;; <DESC>Resets block preserving dynamic properties</DESC>
;;; <ARG>ent - vla or entity of block reference</ARG>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun block-ref-safe-reset (ent / vlist value) 
  (if (= (type ent) 'ENAME) 
    (setq ent (vlax-ename->vla-object ent)))
  (foreach item (vlax-invoke ent 'GetDynamicBlockProperties) 
    (setq vlist (cons (cons (vlax-get item 'PropertyName) (vlax-get item 'Value)) 
                      vlist)))
  (vla-resetblock ent)
  (foreach item (vlax-invoke ent 'GetDynamicBlockProperties) 
    (if (setq value (assoc (vlax-get item 'PropertyName) vlist)) 
      (if (= (vlax-get item 'ReadOnly) 0) 
        (vlax-put item 'Value (cdr value))))))

;;; <LISPDOC>
;;; <SUBR>(block-set-all-explodable)</SUBR>
;;; <DESC>Allows all blocks to be exploded</DESC>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun block-set-all-explodable ( / )
  (vlax-for item (vla-get-blocks (acad-actdoc))
    (vla-put-explodable item :vlax-true)))

;;; <LISPDOC>
;;; <SUBR>(block-get-attribute-value blk attrname)</SUBR>
;;; <DESC>Get block attribute value</DESC>
;;; <ARG>blk - vla block reference</ARG>
;;; <ARG>attrname - attribute name</ARG>
;;; <RET>given attribute value</RET>
;;; </LISPDOC>
(defun block-get-attribute-value (blk attrname / atts)
  (setq atts (vlax-invoke blk 'getAttributes))
  (setq atts (vl-remove-if-not (function (lambda (x) (= (vla-get-TagString x) attrname))) atts))
  (if atts
    (vla-get-textstring (car atts))
    nil)
  )

;;; <LISPDOC>
;;; <SUBR>(block-set-attribute-value blk attrname value)</SUBR>
;;; <DESC>Set block attribute value</DESC>
;;; <ARG>blk - vla block reference</ARG>
;;; <ARG>attrname - attribute name</ARG>
;;; <ARG>value - value to set</ARG>
;;; <RET>given attribute value</RET>
;;; </LISPDOC>
(defun block-set-attribute-value (blk attrname value / atts)
  (setq atts (vlax-invoke blk 'getAttributes))
  (setq atts (vl-remove-if-not (function (lambda (x) (= (vla-get-TagString x) attrname))) atts))
  (if atts
    (vla-put-textstring (car atts) value)
    nil)
  )
  
(defun block-get-attributelist (vlablk / )
  (mapcar (function (lambda (x) (vlax-get x 'TagString))) (vlax-invoke (vlax-ename->vla-object vlablk) 'GetAttributes)))