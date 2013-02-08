(vl-load-com)

(defun reactor-set (rcommand lambda-start lambda-end / )

  (setq reactor-current-command (lambda () rcommand))
   
  (vlr-remove-all :vlr-command-reactor)
  
  (vlr-command-reactor nil '((:vlr-commandWillStart . start_command)))
  (vlr-command-reactor nil '((:vlr-commandEnded . end_command)))

  (setq st (lambda (calling-reactor 

  (defun start_command (calling-reactor start_command_info / )
    (if (wcmatch (reactor-current-command) (car start_command_info))
      (lambda-start)))

  (defun end_command (calling-reactor end_command_info / )
    (if (wcmatch (strcat "*" rcommand) (car end_command_info))
      (lambda-end)))

  (princ))