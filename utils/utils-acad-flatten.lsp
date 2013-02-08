; Callback subroutines for entities
(defun acad-entities-flatten-callback ()
  (list
    (cons 'AcDb3dPolyline			'acad-flatten-coordinates)
    (cons 'AcDbArc 					'acad-flatten-center)
    (cons 'AcDbAttribute 			'acad-flatten-inspoint)
    (cons 'AcDbAttributeDefinition	'acad-flatten-inspoint)
    (cons 'AcDbBlockTableRecord		'acad-flatten-inspoint)
    (cons 'AcDbBlockReference		'acad-flatten-inspoint)
    (cons 'AcDbCircle				'acad-flatten-center)
    (cons 'AcDbEllipse				'acad-flatten-center)
    (cons 'AcDbHatch				'acad-flatten-elevation)
    (cons 'AcDbLine					'acad-flatten-2point)
    (cons 'AcDbMline				'acad-flatten-elevation)
    (cons 'AcDbMText				'acad-flatten-inspoint)
    (cons 'AcDbPoint				'acad-flatten-coordinates)
    (cons 'AcDbPolyline				'acad-flatten-elevation)
    (cons 'AcDbRasterImage			'acad-flatten-inspoint)
    (cons 'AcDbSpline				'acad-flatten-controlpoints)
    (cons 'AcDbTable				'acad-flatten-inspoint)
    (cons 'AcDbText					'acad-flatten-inspoint)))

; Groups list of coordinates (x1 y1 z1 x2 y2 z2) into ((x1 y1 z1)(x2 y2 z2))
(defun acad-coords-list-group (lst /)
  (if lst
    (cons
      (list
	(car lst)
	(cadr lst)
	(caddr lst))
      (acad-coords-list-group (cdddr lst)))))

; Nulls every third coordiate in list if coordinates ((x1 y1 z1)(x2 y2 z2))
(defun acad-coords-list-z-null (lst /)
  (if lst
    (cons
      (list
	(caar lst)
	(cadar lst)
	0.0)
      (acad-coords-list-z-null (cdr lst)))))

; Splashes grouped list of coordinates into (x1 y1 z1 x2 y2 z2)
(defun acad-coords-list-splash (lst /)
  (if lst (list-flatten lst)))

; Nulls third coordinate in list of coordinates or one point
(defun acad-coords-z-null (lst /)
  (if (listp lst)
    (cond
      ((and (<= 2 (length lst) 3) (atom (car lst)))
       (list (car lst) (cadr lst) 0.0))
      ((> (length lst) 3)
       (acad-coords-list-splash
	 (acad-coords-list-z-null
	   (acad-coords-list-group lst)))))))

; Returns AcDb Entity type
(defun acad-entity-type (ent)
  (cond
    ((= (type ent) 'ENAME)
     (read (vlax-get (vlax-ename->vla-object ent) 'ObjectName)))
    ((= (type ent) 'VLA-OBJECT)
     (read (vlax-get ent 'ObjectName)))
    (t nil)))

; Return flatten callback subroutine for entity
(defun acad-entity-get-flatten-callback (ent / subr)
  (setq subr (cdr(assoc (acad-entity-type ent) (acad-entities-flatten-callback))))
  (if (member subr (atoms-family 0))
    subr
    nil))
 
; Flatten for Arc, Circle etc...
(defun acad-flatten-center (ent)
  (vlax-put ent 'Center (acad-coords-z-null (vlax-get ent 'Center))))

; Flatten for Attribute, Attribute, BlockRef, ExternalReference etc...
(defun acad-flatten-inspoint (ent)
  (vlax-put ent 'InsertionPoint (acad-coords-z-null (vlax-get ent 'InsertionPoint))))

; Flatten for Line
(defun acad-flatten-2point (ent)
  (vlax-put ent 'StartPoint (acad-coords-z-null (vlax-get ent 'StartPoint)))
  (vlax-put ent 'EndPoint (acad-coords-z-null (vlax-get ent 'EndPoint))))

; Flatten for Poly
(defun acad-flatten-elevation (ent)
  (vlax-put ent 'Elevation 0.0))

; Flatten for 3DPoly
(defun acad-flatten-coordinates (ent)
  (vlax-put ent 'Coordinates (acad-coords-z-null (vlax-get ent 'Coordinates))))

; Flatten for Spline
(defun acad-flatten-controlpoints (ent)
  (vlax-put ent 'ControlPoints (acad-coords-z-null (vlax-get ent 'ControlPoints))))    

; Flatten object
(defun acad-flatten-object (ent / subr)
  (if (setq subr (acad-entity-get-flatten-callback ent))
    (apply subr (list ent))))

; Flatten vla-collection of objects
(defun acad-flatten-collection (coll)
  (vlax-for item coll
	(if (acad-layer-can-be-changed (vla-get-layer item))
     (acad-flatten-object item))))

; Check whether layer is changeable or not
(defun acad-layer-can-be-changed (layer / opts)
  (setq opts (cdr (assoc 70 (entget (tblobjname "LAYER" layer)))))
  (if (> (+ (logand 1 opts) (logand 4 opts)) 0) 
    nil
	T))
	
; Return Block definition VLA-object instead of Block reference vla-object
(defun acad-block-definition (blk)
  (if (= (type blk) 'ENAME)
    (setq blk (vlax-ename->vla-object blk)))
  (if
    (and
      (= (type blk) 'VLA-OBJECT)
      (= (vlax-get blk 'ObjectName) "AcDbBlockReference"))
    (vla-item (vla-get-blocks (acad-actdoc)) (vlax-get blk 'EffectiveName))
    nil))

; Main subroutine to flatten drawing
(defun flatten-objects ()
  (vlax-for coll (vla-get-blocks (acad-actdoc))
    (if
      (and
	(vlax-property-available-p coll 'Name)
	(/= (vlax-get coll 'Name) "*Paper_Space"))
      (acad-flatten-collection coll))))
	  
(defun flatten-selected ( / selset ent k)
  (if (not (setq selset (cadr(ssgetfirst))))
    (setq selset (ssget)))
  
  (if selset
    (repeat (setq k (sslength selset))
	  (setq ent (vlax-ename->vla-object (ssname selset (setq k (1- k)))))
	  (acad-flatten-object ent))))
    
	