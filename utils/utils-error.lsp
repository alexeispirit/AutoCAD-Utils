;;;<LISPDOC>
;;;<SUBR>(error-try test catch finally</SUBR>
;;;<DESC>Error trap subroutine \
;;; like try-catch-finally \
;;; syntax: \
;;; (try \
;;; 	'(lambda () subroutines to execute) \
;;;	'(lambda (ex) subroutines to process errors) \
;;;	'(lambda () subroutines to execute anyway))</DESC>
;;;<ARG>test - lambda subr to execute</ARG>
;;;<ARG>catch - lambda subr to process errors</ARG>
;;;<ARG>finally - subroutines to execute anyway</ARG>
;;;<RET>test subr result or catch result</RET>
;;;</LISPDOC>
(defun error-try (test catch finally / res)
  (setq res (vl-catch-all-apply test))
  (apply finally (list ))
  (if (and (vl-catch-all-error-p res) catch)
    (apply catch (list (vl-catch-all-error-message res)))
    res))