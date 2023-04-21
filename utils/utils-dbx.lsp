;;; <LISPDOC>
;;; <SUBR>(dbx-object)</SUBR>
;;; <DESC>Get ObjectDBX object point</DESC>
;;; <RET>ObjectDBX.AxDbDocument pointer</RET>
;;; </LISPDOC>
(defun dbx-object ( / )
  (vla-getinterfaceobject (acad-object)
    (strcat "ObjectDBX.AxDbDocument."
            (substr (getvar "ACADVER") 1 2))))

;;; <LISPDOC>
;;; <SUBR>(dbx-copy)</SUBR>
;;; <DESC>Copy all object from dbx document to active document</DESC>
;;; <ARG>dbx - ObjectDBX pointer</ARG>
;;; <ARG>src - Source collection in DBX document</ARG>
;;; <ARG>dst - Destionation collection</ARG>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun dbx-copy (dbx src dst / item styles sa)
  (setq styles (list))
  (vlax-for item src
    (setq styles (append styles (list item))))
  (setq sa (vlax-safearray-fill
             (vlax-make-safearray
               vlax-vbObject
               (cons 0 (1- (vla-get-count src)))) styles))
  (vla-copyobjects dbx sa dst))

;;; <LISPDOC>
;;; <SUBR>(dbx-import)</SUBR>
;;; <DESC>Import textstyle, dimstyles, linetypes and layers from DBX to active document</DESC>
;;; <ARG>filename - DWG filename to open as DBX</ARG>
;;; <RET>None</RET>
;;; </LISPDOC>
(defun dbx-import (filename / name dbx)
  (setq dbx (dbx-object))
  (vla-open dbx filename)
  (foreach name '("textstyles" "dimstyles" "linetypes" "layers")
    (dbx-copy dbx
              (apply (read (strcat "vla-get-" name)) (list dbx))
              (apply (read (strcat "vla-get-" name)) (list (acad-actdoc)))))
  (vlax-release-object dbx)
)

