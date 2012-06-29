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