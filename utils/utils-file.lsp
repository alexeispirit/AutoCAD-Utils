;;; <LISPDOC>
;;; <SUBR>(file-info)</SUBR>
;;; <DESC>Retrives file information</DESC>
;;; <RET>list of FILEINFO structure \
;;; (("FILE" ...) \
;;;  ("PATH" ...) \
;;;  ("DIR"  ...))</RET>
;;; </LISPDOC>
(defun file-info (/ fullname dir)
  (setq fullname
	 (vla-get-fullname
	   (vla-get-activedocument
	     (vlax-get-acad-object))))
  (if (/= "" fullname)
    (list
      (cons "FILE"
	    (strcat
	      (vl-filename-base fullname)
	      (vl-filename-extension fullname)))
      (cons "PATH"
	    (setq dir (vl-filename-directory fullname)))
      (cons "DIR"
	    (substr dir
	      (+ 2
		(string-search-reverse "\\" dir)))))))

;;; <LISPDOC>
;;; <SUBR>(file-from-projectwise fileinfo)</SUBR>
;;; <DESC>Checks whether file in PW or not</DESC>
;;; <ARG>fileinfo - list of FILEINFO structure</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun file-from-projectwise (fileinfo / dir)
  (setq dir (cdr (assoc "DIR" fileinfo)))
  (if
    (and dir
    (or	(wcmatch dir "dms#*")
	(wcmatch dir "d#*")))
    T
    nil))

;;; <LISPDOC>
;;; <SUBR>(file-read-config file)</SUBR>
;;; <DESC>Reads config file and returns list of values</DESC>
;;; <ARG>file - path to file</ARG>
;;; <RET>list of read values</RET>
;;; </LISPDOC>
(defun file-read-config (file / fh line conf_list)
  (if (findfile file)
    (progn
      (setq fh (open (findfile file) "r"))
      (setq conf_list (list ))
      (while (setq line (read-line fh))
        (if (/= (vl-string-search ";" line) 0)
          (setq conf_list (append conf_list (list(read line))))))
      (close fh)
      conf_list)))

;;; <LISPDOC>
;;; <SUBR>(file-read-value key config)</SUBR>
;;; <DESC>Reads value from (key . value) config file</DESC>
;;; <ARG>key - key to read</ARG>
;;; <ARG>config - config list to read from</ARG>
;;; <RET>matching value</RET>
;;; </LISPDOC>
(defun file-read-value (key config / )
  (cdr (assoc key config)))

;;; <LISPDOC>
;;; <SUBR>(file-qrcode-generate string)</SUBR>
;;; <DESC>Generates QRcode from string</DESC>
;;; <ARG>string - string to encode</ARG>
;;; <RET>path to QRcode image</RET>
;;; </LISPDOC>
(defun file-qrcode-generate (string / fname)
  (dos_execute
    (strcat
      "qrcode.exe -o "
      (setq fname (vl-filename-mktemp "qr-" (getenv "TEMP") ".png"))
      " -s 10 "
      string)
    3)
  fname)