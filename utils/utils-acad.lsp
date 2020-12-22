;;; <LISPDOC>
;;; <SUBR>(acad-version)</SUBR>
;;; <DESC>Retrives autocad version as float</DESC>
;;; <RET>Float AutoCAD version number</RET>
;;; </LISPDOC>
(defun acad-version () 
  (atof (substr (getvar "ACADVER") 1 4))
)

;;; <LISPDOC>
;;; <SUBR>(acad-object)</SUBR>
;;; <DESC>Get AutoCAD.Application object pointer</DESC>
;;; <RET>AutoCAD.Application pointer</RET>
;;; </LISPDOC>
(defun acad-object () 
  (vlax-get-acad-object)
)

;;; <LISPDOC>
;;; <SUBR>(acad-actdoc)</SUBR>
;;; <DESC>Get ActiveDocument pointer</DESC>
;;; <RET>ActiveDocument pointer</RET>
;;; </LISPDOC>
(defun acad-actdoc () 
  (vla-get-activedocument (acad-object))
)

;;; <LISPDOC>
;;; <SUBR>(acad-dwgname)</SUBR>
;;; <DESC>Get activedocument drawing name</DESC>
;;; <RET>Current document name</RET>
;;; </LISPDOC>
(defun acad-dwgname () 
  (vlax-get (acad-actdoc) 'Name)
)

;;; <LISPDOC>
;;; <SUBR>(acad-paperspace doc)</SUBR>
;;; <DESC>Get PaperSpace pointer</DESC>
;;; <ARG>doc - activedocument pointer (auto if nil)</ARG>
;;; <RET>PaperSpace pointer</RET>
;;; </LISPDOC>
(defun acad-paperspace (doc) 
  (if doc 
    (vla-get-paperspace doc)
    (vla-get-paperspace (acad-actdoc))
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-modelspace doc)</SUBR>
;;; <DESC>Get ModelSpace pointer</DESC>
;;; <ARG>doc - activedocument pointer (auto if nil)</ARG>
;;; <RET>ModelSpace pointer</RET>
;;; </LISPDOC>
(defun acad-modelspace (doc) 
  (if doc 
    (vla-get-modelspace doc)
    (vla-get-modelspace (acad-actdoc))
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-currentspace doc)</SUBR>
;;; <DESC>Get CurrentSpace pointer</DESC>
;;; <ARG>doc - activedocument pointer (auto if nil)</ARG>
;;; <RET>Current Paper or Model space pointer</RET>
;;; </LISPDOC>
(defun acad-currentspace (doc /) 
  (if (not doc) 
    (setq doc (acad-actdoc))
  )
  (if (= (getvar "TILEMODE") 1) 
    (acad-modelspace doc)
    (acad-paperspace doc)
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-actlayout-name doc)</SUBR>
;;; <DESC>Get Active Layout name</DESC>
;;; <ARG>doc - activedocument pointer (auto if nil)</ARG>
;;; <RET>Active Layout name</RET>
;;; </LISPDOC>
(defun acad-actlayout-name (doc) 
  (if (not doc) 
    (setq doc (acad-actdoc))
  )
  (vlax-get (vla-get-activelayout doc) 'Name)
)

;;; <LISPDOC>
;;; <SUBR>(acad-dump)</SUBR>
;;; <DESC>Dump vla-object</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun acad-dump (vla_object) 
  (vlax-dump-object vla_object T)
)

;;; <LISPDOC>
;;; <SUBR>(acad-vlasel)</SUBR>
;;; <DESC>Get vla-object from entsel</DESC>
;;; <RET>vla-object</RET>
;;; </LISPDOC>
(defun acad-vlasel () 
  (vlax-ename->vla-object (car (entsel)))
)

;;; <LISPDOC>
;;; <SUBR>(acad-entsel)</SUBR>
;;; <DESC>Entity selection</DESC>
;;; <RET>entity</RET>
;;; </LISPDOC>
(defun acad-entsel () 
  (car (entsel))
)

;;; <LISPDOC>
;;; <SUBR>(acad-ent2vla entity)</SUBR>
;;; <DESC>Get vla-object from entity</DESC>
;;; <ARG>entity - entity to convert</ARG>
;;; <RET>vla-object</RET>
;;; </LISPDOC>
(defun acad-ent2vla (entity) 
  (vlax-ename->vla-object entity)
)

;;; <LISPDOC>
;;; <SUBR>(acad-selset-onscreen etype)</SUBR>
;;; <DESC>Select objects on screen</DESC>
;;; <ARG>etype - type of entities</ARG>
;;; <RET>VLA Selection Set</RET>
;;; </LISPDOC>
(defun acad-selset-onscreen (etype / sel) 
  (setq sel (vla-get-activeselectionset (acad-actdoc)))
  (vla-selectonscreen sel 
                      (vlax-safearray-fill 
                        (vlax-make-safearray vlax-vbinteger '(0 . 0))
                        '(0)
                      )
                      (vlax-safearray-fill 
                        (vlax-make-safearray vlax-vbvariant '(0 . 0))
                        (list etype)
                      )
  )
  sel
)

;;; <LISPDOC>
;;; <SUBR>(acad-selset-universal)</SUBR>
;;; <DESC>Create selection set \
;;; with user prompt or selected objects</DESC>
;;; <RET>Selection Set</RET>
;;; </LISPDOC>
(defun acad-selset-universal (/ selset) 
  (if (not (setq selset (cadr (ssgetfirst)))) 
    (setq selset (ssget))
  )
  selset
)

;;; <LISPDOC>
;;; <SUBR>(acad-ent-boundingbox entity)</SUBR>
;;; <DESC>Get entity bounding box coordinates</DESC>
;;; <ARG>entity - entity to get coords (vla or ent)</ARG>
;;; <RET>list of points (min max)</RET>
;;; </LISPDOC>
(defun acad-ent-boundingbox (entity / minp maxp) 
  (if (= 'ENAME (type entity)) 
    (setq entity (acad-ent2vla entity))
  )
  (if (= 'VLA-OBJECT (type entity)) 
    (vla-GetBoundingBox entity 'minp 'maxp)
  )
  (if (and minp maxp) 
    (list 
      (vlax-safearray->list minp)
      (vlax-safearray->list maxp)
    )
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-selset-boundingbox sel)</SUBR>
;;; <DESC>Get selection set bounding box</DESC>
;;; <ARG>sel - VLA selection set</ARG>
;;; <RET>list of points (min max)</RET>
;;; </LISPDOC>
(defun acad-selset-boundingbox (sel / minp maxp minlst maxlst) 
  (vlax-for item 
            sel
            (vla-GetBoundingBox item 'minp 'maxp)
            (setq minlst (cons (vlax-safearray->list minp) minlst)
                  maxlst (cons (vlax-safearray->list maxp) maxlst)
            )
  )
  (list (apply 'mapcar (cons 'min minlst)) 
        (apply 'mapcar (cons 'max maxlst))
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-list-boundingbox entlist)</SUBR>
;;; <DESC>Get entities list bounding box</DESC>
;;; <ARG>entlist - list of entities</ARG>
;;; <RET>list of points (min max)</RET>
;;; </LISPDOC>
(defun acad-list-boundingbox (entlist / boxlst minlst maxlst) 
  (foreach item entlist 
    (setq boxlst (acad-ent-boundingbox item))
    (setq minlst (cons (car boxlst) minlst)
          maxlst (cons (cadr boxlst) maxlst)
    )
  )
  (list (apply 'mapcar (cons 'min minlst)) 
        (apply 'mapcar (cons 'max maxlst))
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-boundingbox-blc clist)</SUBR>
;;; <DESC>Get the bottom left corner coordinates</DESC>
;;; <ARG>clist - coordinates list</ARG>
;;; <RET>List of coordinates</RET>
;;; </LISPDOC>
(defun acad-boundingbox-blc (clist /) 
  (car clist)
)

;;; <LISPDOC>
;;; <SUBR>(acad-boundingbox-ulc clist)</SUBR>
;;; <DESC>Get the upper left corner coordinates</DESC>
;;; <ARG>clist - coordinates list</ARG>
;;; <RET>List of coordinates</RET>
;;; </LISPDOC>
(defun acad-boundingbox-ulc (clist /) 
  (list (caar clist) (cadadr clist) (last (car clist)))
)

;;; <LISPDOC>
;;; <SUBR>(acad-boundingbox-urc clist)</SUBR>
;;; <DESC>Get the upper right corner coordinates</DESC>
;;; <ARG>clist - coordinates list</ARG>
;;; <RET>List of coordinates</RET>
;;; </LISPDOC>
(defun acad-boundingbox-urc (clist /) 
  (cadr clist)
)

;;; <LISPDOC>
;;; <SUBR>(acad-boundingbox-brc clist)</SUBR>
;;; <DESC>Get the bottom right corner coordinates</DESC>
;;; <ARG>clist - coordinates list</ARG>
;;; <RET>List of coordinates</RET>
;;; </LISPDOC>
(defun acad-boundingbox-brc (clist /) 
  (list (caadr clist) (cadar clist) (last (car clist)))
)

;;; <LISPDOC>
;;; <SUBR>(acad-ent-get-layer ent)</SUBR>
;;; <DESC>Get entity layer</DESC>
;;; <ARG>ent - ENAME entity</ARG>
;;; <RET>ENAME layer</RET>
;;; </LISPDOC>
(defun acad-ent-get-layer (ent /) 
  (if (= 'ENAME (type ent)) 
    (tblobjname "LAYER" (cdr (assoc 8 (entget ent))))
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-ent-get-color ent)</SUBR>
;;; <DESC>Get entity color</DESC>
;;; <ARG>ent - ENAME entity</ARG>
;;; <RET>INT entity color</RET>
;;; </LISPDOC>
(defun acad-ent-get-color (ent / color) 
  (if (= 'ENAME (type ent)) 
    (progn 
      (setq color (cdr (assoc 62 (entget ent))))
      (if (or (null color) (zerop color)) 
        (setq color (acad-ent-get-color (acad-ent-get-layer ent)))
        color
      )
    )
  )
)
       
;;; <LISPDOC>
;;; <SUBR>(acad-ent-get-ltype ent)</SUBR>
;;; <DESC>Get entity linetype</DESC>
;;; <ARG>ent - ENAME entity</ARG>
;;; <RET>STR entity linetype</RET>
;;; </LISPDOC>
(defun acad-ent-get-ltype (ent / lt) 
  (if (= 'ENAME (type ent)) 
    (progn 
      (setq lt (cdr (assoc 6 (entget ent))))
      (if (or (null lt) (= "ByBlock" lt)) 
        (setq lt (acad-ent-get-ltype (acad-ent-get-layer ent)))
        lt
      )
    )
  )
)

;;; <LISPDOC>
;;; <SUBR>(acad-ent-get-lweight ent)</SUBR>
;;; <DESC>Get entity lineweight</DESC>
;;; <ARG>ent - ENAME entity</ARG>
;;; <RET>INT entity lineweight</RET>
;;; </LISPDOC>
(defun acad-ent-get-lweight (ent / lw) 
  (if (= 'ENAME (type ent)) 
    (progn 
      (setq lw (cdr (assoc 370 (entget ent))))
      (cond 
        ((null lw)
         (setq lw (acad-ent-get-lweight (acad-ent-get-layer ent)))
        )
        ((= -3 lw)
         (setq lw 25)
        )
        (t lw)
      )
    )
  )
)

;;;<LISPDOC>
;;;<SUBR>(acad-objectid vlent)</SUBR>
;;;<DESC>Get ObjectId inspite of system bit</DESC>
;;;<ARG>vla-object</ARG>
;;;<RET>ObjectID</RET>
;;;</LISPDOC>
(defun acad-objectid (vlent / prop) 
  (if (= (type vlent) 'VLA-OBJECT) 
    (vlax-get-property vlent 
                       (strcat "ObjectId" 
                               (if (system-is-x64) "32" "")
                       )
    )
  )
)

;;;<LISPDOC>
;;;<SUBR>(acad-ownerid vlent)</SUBR>
;;;<DESC>Get OwnerId inspite of system bit</DESC>
;;;<ARG>vla-object</ARG>
;;;<RET>OwnerID</RET>
;;;</LISPDOC>
(defun acad-ownerid (vlent / prop) 
  (if (= (type vlent) 'VLA-OBJECT) 
    (vlax-get-property vlent 
                       (strcat "OwnerId" 
                               (if (system-is-x64) "32" "")
                       )
    )
  )
)

;;;<LISPDOC>
;;;<SUBR>(acad-objectidtoobject doc id)</SUBR>
;;;<DESC>Get object by ObjectId</DESC>
;;;<ARG>doc - AutoCAD.Application.Document pointer</ARG>
;;;<ARG>id - objectId</ARG>
;;;<RET>vla-object</RET>
;;;</LISPDOC>
(defun acad-objectidtoobject (doc id) 
  (vlax-invoke doc 
               (strcat "ObjectIDToObject" 
                       (if (system-is-x64) "32" "")
               )
               id
  )
)

;;;<LISPDOC>
;;;<SUBR>(acad-tablelist-ent table)</SUBR>
;;;<DESC>Get the entities list of provided acad TABLE</DESC>
;;;<ARG>table - acad TABLE to search in</ARG>
;;;<RET>List of TABLE entities</RET>
;;;</LISPDOC>
(defun acad-tablelist-ent (table / ent lst) 
  (while (setq ent (tblnext table (null ent))) 
    (setq lst (cons (tblobjname table (cdr (assoc 2 ent))) lst))
  )
  (reverse lst)
)

;;;<LISPDOC>
;;;<SUBR>(acad-tablelist-name table)</SUBR>
;;;<DESC>Get the names list of provided acad TABLE</DESC>
;;;<ARG>table - acad TABLE to search in</ARG>
;;;<RET>List of TABLE names</RET>
;;;</LISPDOC>
(defun acad-tablelist-name (table / ent lst) 
  (while (setq ent (tblnext table (null ent))) 
    (setq lst (cons (cdr (assoc 2 ent)) lst))
  )
  (reverse lst)
)

;;;<LISPDOC>
;;;<SUBR>(acad-tablelist-cons table)</SUBR>
;;;<DESC>Get the (name . entity) list of provided acad TABLE</DESC>
;;;<ARG>table - acad TABLE to search in</ARG>
;;;<RET>List of TABLE (name . entity)</RET>
;;;</LISPDOC>
(defun acad-tablelist-cons (table / ent lst name) 
  (while (setq ent (tblnext table (null ent))) 
    (setq lst (cons (cons (setq name (cdr (assoc 2 ent))) (tblobjname table name)) 
                    lst
              )
    )
  )
  (reverse lst)
)