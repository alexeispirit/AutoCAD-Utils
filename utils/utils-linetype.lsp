;;; <LISPDOC>
;;; <SUBR>(linetype-filelist)</SUBR>
;;; <DESC>Returns list of (form.shx lt1 lt2...)</DESC>
;;; <RET>list of forms and linetypes</RET>
;;; </LISPDOC>
(defun linetype-filelist (/ lt linelist out) 
  (tblnext "LTYPE" T)
  (while (setq lt (tblnext "LTYPE")) 
    (setq linelist (list-massoc 
                     340
                     (entget (tblobjname "LTYPE" (cdr (assoc 2 lt))))))
    (foreach item linelist 
      (setq shx (cdr (assoc 3 (entget item))))
      (setq out (append out (list (list shx (cdr (assoc 2 lt))))))))
  (list-dict out))

;;; <LISPDOC>
;;; <SUBR>(linetype-by-layer)</SUBR>
;;; <DESC>Set all entities linetypes by layer</DESC>
;;; <ARG>lst - list of linetypes to clear</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun linetype-by-layer (lst / blk item func) 
  (setq func (lambda (x lst) 
               (if 
                 (and 
                   (vlax-property-available-p x 'Linetype)
                   (member (vlax-get x 'Linetype) lst))
                 (vlax-put x 
                           'Linetype
                           (if (= (vlax-get x 'ObjectName) "AcDbLayerTableRecord") 
                             "Continuous"
                             "BYLAYER")))))

  (vlax-for blk 
            (vla-get-blocks (acad-actdoc))
            (vlax-for item 
                      blk
                      (func item lst)))
  (vlax-for item 
            (vla-get-layers (acad-actdoc))
            (func item lst)))