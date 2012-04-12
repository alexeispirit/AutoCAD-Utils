(defun projectwise-urn (/ finfo cfg cstring login pwd connect query urn)
  (if (file-from-projectwise (setq finfo (file-info)))
    (progn
      (setq cfg (file-read-config "giprong-settings.conf")
	    cstring (sql-mssql-connection-string
		      (file-read-value "PW-SQL" cfg)
		      (file-read-value "PW-DB" cfg)
		      (setq login (file-read-value "PW-READER" cfg))
		      (setq pwd (file-read-value "PW-PASSWD" cfg))))
      (if (not (setq connect (adolisp_connecttodb cstring login pwd)))
	(adolisp_errorprinter)
	(progn
	  (setq query
		 (strcat
		   "SELECT d.o_docguid
		    FROM dms_doc d
		    INNER JOIN dms_proj p
		    ON d.o_projectno=p.o_projectno
		    WHERE d.o_filename='"
		   (file-read-value "FILE" finfo)
		   "' and p.o_projectcode='"
		   (file-read-value "DIR" finfo)
		   "'"))
	  (setq urn (adolisp_dosql connect query))
	  (adolisp_disconnectfromdb connect)))))
  (if urn
    (strcat
      (file-read-value "PW-URN-STARTS-WITH" cfg)
      (caadr urn)
      (file-read-value "PW-URN-ENDS-WITH" cfg))
    nil))

(defun projectwise-attach-urn-qrcode (urn point / path ps)
  (setq path (file-qrcode-generate urn))
  (setq ps (vla-get-paperspace (vla-get-activedocument (vlax-get-acad-object))))
  (vla-addraster ps path (vlax-3d-point point) 20 0))