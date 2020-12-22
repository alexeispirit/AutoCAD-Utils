;;; <LISPDOC>
;;; <SUBR>(file-info)</SUBR>
;;; <DESC>Retrives file information</DESC>
;;; <RET>list of FILEINFO structure \
;;; (("FILE" ...) \
;;;  ("PATH" ...) \
;;;  ("DIR"  ...))</RET>
;;; </LISPDOC>
(defun file-info (/ fullname dir) 
  (setq fullname (vla-get-fullname 
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
         (or (wcmatch dir "dms#*") 
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
      (setq conf_list (list))
      (while (setq line (read-line fh)) 
        (if (/= (vl-string-search ";" line) 0) 
          (setq conf_list (append conf_list (list (read line))))))
      (close fh)
      conf_list)))

;;; <LISPDOC>
;;; <SUBR>(file-read-value key config)</SUBR>
;;; <DESC>Reads value from (key . value) config file</DESC>
;;; <ARG>key - key to read</ARG>
;;; <ARG>config - config list to read from</ARG>
;;; <RET>matching value</RET>
;;; </LISPDOC>
(defun file-read-value (key config /) 
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

;;; <LISPDOC>
;;; <SUBR>file-join-path (lst)</SUBR>
;;; <DESC>Builds path to file from list</DESC>
;;; <ARG>lst - file path list</ARG>
;;; <RET>string path to file</RET>
;;; </LISPDOC>
(defun file-join-path (lst / out) 
  (setq out (list-flatten 
              (list (car lst) 
                    (mapcar (function (lambda (x) (string-trim-symbols "\\\\" x))) 
                            (cdr lst)))))
  (list-join-to-string out "\\"))

;;; <LISPDOC>
;;; <SUBR>file-load-directory (lst)</SUBR>
;;; <DESC>Loads all files from directory</DESC>
;;; <ARG>dir - directory to load</ARG>
;;; <RET>void</RET>
;;; </LISPDOC>
(defun file-load-directory (dir) 
  (if (findfile dir) 
    (foreach file (vl-directory-files dir "*.lsp" 0) 
      (if (vl-file-directory-p file) 
        (file-load-directory (file-join-path (list dir file)))
        (load (file-join-path (list dir file)) "Not found")))))

;;; <LISPDOC>
;;; <SUBR>(file-netload filename)</SUBR>
;;; <DESC>Load .Net assembly into autocad</DESC>
;;; <ARG>filename - path to dll file</ARG>
;;; <RET>T or nil</RET>
;;; </LISPDOC>
(defun file-netload (filename) 
  (if (findfile filename) 
    (progn 
      (vl-cmdf "_netload" filename)
      T)
    nil))