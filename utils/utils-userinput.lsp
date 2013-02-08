;;;<LISPDOC>
;;;<SUBR>(userinput-getkword-extended title lst)</SUBR>
;;;<DESC>Provides getkword functionality to list \
;;; consists of various strings</DESC>
;;;<ARG>title - title to ask user for input (string)</ARG>
;;;<ARG>lst - list of strings to ask from</ARG>
;;;<RET>chosen string from list</RET>
;;;</LISPDOC>
(defun userinput-getkword-extended (title lst / k initstr value)
  (textscr)
  (setq initstr "")
  (setq k 0)
  (foreach item lst
    (princ (strcat "\n   " (itoa (setq k (1+ k))) " - " item))
    (setq initstr (strcat initstr " " (itoa k))))
  (initget 1 initstr)
  (nth (1- (atoi (getkword (strcat "\n" title)))) lst))

;;;<LISPDOC>
;;;<SUBR>(userinput-string-default string default)</SUBR>
;;;<DESC>Generates string with default value \
;;; for get... functions</DESC>
;;;<ARG>string - string to show<ARG>
;;;<ARG>default - default value for prompt</ARG>
;;;<RET>prompt string with default</RET>
;;;</LISPDOC>  
(defun userinput-string-default (string default)
  (strcat string " <" (vl-princ-to-string default) ">: "))

;;;<LISPDOC>
;;;<SUBR>(userinput-prompt-default string default)</SUBR>
;;;<DESC>Prompts for value with default</DESC>
;;;<ARG>string - string to show<ARG>
;;;<ARG>default - default value for prompt</ARG>
;;;<RET>value or default</RET>
;;;</LISPDOC>    
(defun userinput-prompt-default (string default / pstring out)
  (setq pstring (userinput-string-default string default))
  (setq out
	 (cond
	   ((= (type default) 'INT) (getint pstring))
	   ((= (type default) 'STR) (getstring pstring))
	   ((= (type default) 'REAL) (getreal pstring))
	   ((= (type default) 'LST) (getpoint pstring))))
  (if (and
	(= (type default) 'STR)
	(= (strlen out) 0))
    (setq out nil))
  (if out
    out
    default))
