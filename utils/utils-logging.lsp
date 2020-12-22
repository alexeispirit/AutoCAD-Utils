(defun utils-logging-on (file / old) 
  (vlr-remove-all :vlr-lisp-reactor)
  (setq *lisprun*   nil
        *logfile*   file
        *preverror* *error*)

  (vlr-editor-reactor "LOGGING" 
                      '((:vlr-lispwillstart . utils-logging-save-info)
                        (:vlr-commandwillstart . utils-logging-save-info)))


  (defun *error* (errmsg / fh msg) 
    (setq fh  (open *logfile* "a")
          msg (strcat (vl-princ-to-string *lisprun*) 
                      ": "
                      (vl-princ-to-string errmsg)))
    (print msg)
    (write-line msg fh)
    (close fh))
  *preverror*)

(defun utils-logging-off (old) 
  (setq *logfile* nil
        *error*   (if old old *olderror*)))

(defun utils-logging-save-info (call callback) 
  (setq *lisprun* callback))