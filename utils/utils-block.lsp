
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
        
(defun block-set-all-explodable ( / )
  (vlax-for item (vla-get-blocks (acad-actdoc))
    (vla-put-explodable item :vlax-true)))

(defun block-get-attribute-value (blk attrname / atts)
  (setq atts (vlax-invoke blk 'getAttributes))
  (setq atts (vl-remove-if-not (function (lambda (x) (= (vla-get-TagString x) attrname))) atts))
  (if atts
    (vla-get-textstring (car atts))
    nil)
  )

(defun block-set-attribute-value (blk attrname value / atts)
  (setq atts (vlax-invoke blk 'getAttributes))
  (setq atts (vl-remove-if-not (function (lambda (x) (= (vla-get-TagString x) attrname))) atts))
  (if atts
    (vla-put-textstring (car atts) value)
    nil)
  )
  
(defun block-get-attributelist (vlablk / )
  (mapcar (function (lambda (x) (vlax-get x 'TagString))) (vlax-invoke (vlax-ename->vla-object vlablk) 'GetAttributes)))