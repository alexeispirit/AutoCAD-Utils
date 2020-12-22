;;; <LISPDOC>
;;; <SUBR>(reactor-set-on-command rcommand-list lambda-start lambda-end)</SUBR>
;;; <DESC>Set reactor on specified command start and end</DESC>
;;; <ARG>rcommand-list - commands to check</ARG>
;;; <ARG>lambda-start - command start subroutine</ARG>
;;; <ARG>lambda-end - command end subroutine</ARG>
;;; <RET>nil</RET>
;;; </LISPDOC>
(defun reactor-set-on-command (rcommand-list lambda-start lambda-end /) 

  (setq reactor-commands     rcommand-list
        reactor-lambda-start lambda-start
        reactor-lambda-end   lambda-end)

  (vlr-remove-all :vlr-command-reactor)

  (vlr-command-reactor nil '((:vlr-commandWillStart . reactor-start-command)))
  (vlr-command-reactor nil '((:vlr-commandEnded . reactor-end-command)))

  (defun reactor-test-command (reactor_command_info) 
    (vl-member-if 
      '(lambda (x) 
         (wcmatch 
           (strcase reactor_command_info)
           (strcase (strcat "*" x "*"))))
      reactor-commands))

  (defun reactor-start-command (calling-reactor start_command_info /) 
    (if (reactor-test-command (car start_command_info)) 
      (reactor-lambda-start)))

  (defun reactor-end-command (calling-reactor end_command_info /) 
    (if (reactor-test-command (car end_command_info)) 
      (reactor-lambda-end)))

  (princ))