;;; <LISPDOC>
;;; <SUBR>(utils-load)</SUBR>
;;; <DESC>Load lisp files in provided order. Add utils folder to your Support path</DESC>
;;; <RET>Nothing</RET>
;;; </LISPDOC>  
(defun utils-load (/) 
  (setq *utils_load_order* (list "utils-regexp.lsp" "utils-string.lsp" 
                                 "utils-list.lsp" "utils-system.lsp" "utils-file.lsp" 
                                 "utils-sql.lsp" "utils-projectwise.lsp" "utils-acad.lsp" 
                                 "utils-date.lsp" "utils-userinput.lsp" "utils-error.lsp" 
                                 "utils-reactor.lsp" "utils-math.lsp" "utils-linetype.lsp" 
                                 "utils-editor.lsp" "utils-layer.lsp" "utils-prosteel.lsp" 
                                 "utils-entity.lsp" "utils-block.lsp" "utils-sset.lsp" "utils-dict.lsp"
                                 "utils-matrix.lsp" "utils-vector.lsp"
                           )
  )

  (foreach loading *utils_load_order* 
    (load (findfile loading) nil)
  )
)

(utils-load)