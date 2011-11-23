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
;;; <SUBR>(prosteel-remove-dicts)</SUBR>
;;; <DESC>Remove all prosteel dictionaries (proxy-objects)</DESC>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun prosteel-remove-dicts ( / dict)
  (vlax-for dict (acad-dicts (acad-actdoc))
    (if (vlax-property-available-p dict 'name)
      (if (member (vla-get-name dict) (prosteel-dicts))
	(vla-delete dict)))))



