(setq *utils_load_order*
       (list
	 "utils-regexp.lsp"
	 "utils-string.lsp"
	 "utils-list.lsp"
	 "utils-system.lsp"
	 "utils-file.lsp"
	 "utils-sql.lsp"
	 "utils-projectwise.lsp"
	 "acad-utils"))

(foreach loading *utils_load_order*
  (load loading))
