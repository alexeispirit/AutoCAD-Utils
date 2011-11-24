;;; <LISPDOC>
;;; <SUBR>(acad-actdoc)</SUBR>
;;; <DESC>AutoCAD.Application.ActiveDocument pointer</DESC>
;;; <RET>ActiveDocument pointer</RET>
;;; </LISPDOC>
(defun acad-actdoc ()
  (vla-get-ActiveDocument (vlax-get-acad-object)))

;;; <LISPDOC>
;;; <SUBR>(acad-dicts)</SUBR>
;;; <DESC>AutoCAD ActiveDocument dictionaries collection</DESC>
;;; <ARG>actdoc - AutoCAD ActiveDocument pointer</ARG>
;;; <RET>vlax Dictionaries collection</RET>
;;; </LISPDOC>
(defun acad-dicts (actdoc)
  (if actdoc
    (vla-get-dictionaries actdoc)))

;;; <LISPDOC>
;;; <SUBR>(prosteel-dicts)</SUBR>
;;; <DESC>Prosteel dictionaries names</DESC>
;;; <RET>List of Bentley Prosteel/Structural dictionaries</RET>
;;; </LISPDOC>
(defun prosteel-dicts ( / )
  (list "Ks_ShapeRefDictionary" "Ks_DetailStyleDictionary" "Ks_XRecordDictionary" "Ks_GroupDataDictionary"))

;;; <LISPDOC>
;;; <SUBR>(prosteel-check)</SUBR>
;;; <DESC>Check is there any prosteel dictionaries in drawing database</DESC>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun prosteel-check (/ result)
  (vlax-for dict (acad-dicts (acad-actdoc))
    (if (vlax-property-available-p dict 'Name)
      (if (member (vlax-get dict 'Name) (prosteel-dicts))
	(setq result T))))
  result)

;;; <LISPDOC>
;;; <SUBR>(prosteel-remove-dicts)</SUBR>
;;; <DESC>Remove all prosteel dictionaries (proxy-objects)</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun prosteel-remove-dicts ( / dict)
  (vlax-for dict (acad-dicts (acad-actdoc))
    (if (vlax-property-available-p dict 'Name)
      (if (member (vlax-get dict 'Name) (prosteel-dicts))
	(vla-delete dict)))))