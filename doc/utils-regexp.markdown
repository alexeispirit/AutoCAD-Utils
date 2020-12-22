# ../AutoCAD-Utils/utils/utils-regexp.lsp
## (regexp-regapp)
Get VBScript.RegExp pointer
* No arguments
returns: VBScript.RegExp pointer
## (regexp-match regexp_object pattern test_string is_global case_sensitive)
Test string match with Regexp
* regexp_object - VBScript.RegExp pointer
* pattern - regexp pattern
* test_string - string to test
* is_global - global match key (g)
* case_sensitive - case sensitive key (i)
returns: T if matches <br/> nil otherwise
## (regexp-replace regexp_object pattern replace_string test_string is_global case_sensitive)
Replace Regexp pattern with string
* regexp_object - VBScript.RegExp pointer
* pattern - regexp pattern
* replace_string - string replacement
* test_string - string to test
* is_global - global match key (g)
* case_sensitive - case sensitive key (i)
returns: String after replace
## (regexp-execute regexp_object pattern test_string is_global case_sensitive)
Execute regexp and return collection of found strings
* regexp_object - VBScript.RegExp pointer
* pattern - regexp pattern
* test_string - string to test
* is_global - global match key (g)
* case_sensitive - case sensitive key (i)
returns: List of found strings ((String Index Length)...)
## (regexp-execute-to-plain-list regexp_object regexp string)
Search for regexp in string
* regexp_object - VBScript.RegExp
* regexp - regexp to test
* string - string to test
returns: list of found strings
