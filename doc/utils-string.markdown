# G:\acadext\AutoCAD-Utils\utils\utils-string.lsp
##(string-remove-pattern regexp_object pattern str)
Remove pattern from string
* pattern - pattern to remove
* str - string to clear
returns: cleared string
##(string-format-list)
List of MText format symbols
* No arguments
returns: list of MText string format symbols
##(string-remove-format regexp str)
Remove all format characters from MText string
* regexp_object - VBScript.RegExp pointer
* str - MText string to clear
returns: cleared string
##(string-is-null-or-empty str)
Check whether string is empty or nil
* str - string to test
returns: T - if empty <br/> nil - otherwise
##(string-is-a-comment str)
Check whether string is a comment ()
* str - string to test
returns: T - if comment <br/> nil - otherwise
##(string-split-to-list str splitter)
Split string into list by pattern
* str - string to split
* splitter - delimiter
returns: list of splitted strings
##(string-make-pair str)
Split string by space and check if it has 2 elements <br/> ("GOST 21.1001-2009") -> ("GOST" "21.1001-2009")
* str - string to split
returns: list of splitted strings if list has 2 elements <br/> nil otherwise
##(string-remove-with-whitelist-rule str rule)
Remove trash from string with whitelist rule
* str - string to clear
* rule - rule from whitelist to apply
returns: cleared string
##(string-remove-with-whitelist-total str whitelist)
Remove trash from string with whitelist
* str - string to clear
* whitelist - whitelist to apply
returns: cleared string
##(string-search-reverse item str)
Reverse search in string
* item - item to look for
* str - string to search in
returns: item index or nil
##(string-search item str)
search in string, alias to vl-string-search
* item - item to look for
* str - string to search in
returns: item index or nil
##(string-contains str template)
Check if string contains template
* str - string to search in
* template - template to search
returns: T or nil
##(string-regexp-replace-fast pattern replacer str)
Replace regexp pattern with string. Autoregister application
* pattern - regexp pattern
* replacer - string replacement
* str - string to search in
returns: new string
##(string-trim-symbols pattern str)
Trim symbols from string. Autoregister application
* pattern - regexp pattern
* str - string to search in
returns: new string
