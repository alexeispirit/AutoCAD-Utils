# d:/src/AutoCAD-Utils\utils\utils-list.lsp
## (list-join-to-string strlist splitter)
Join list of strings into string
* strlist - list of strings
* splitter - delimiter
returns: Joined string
## (list-remove-duplicates lst)
Remove duplicates from list
* lst - list to proceed
returns: cleared list
## (list-remove-duplicates-prec lst prec)
Remove duplicates from list with precision
* lst - list to proceed
* prec - precision
returns: cleared list
## (list-union list1 list2)
Union list function
* list1, list2 - lists to union
returns: united list
## (list-intersect list1 list2)
Intersect list function
* list1, list2 - lists to intersect
returns: intersected list
## (list-substract list1 list2)
Substract list function
* list1, list2 - lists to substract
returns: substracted list
## (list-xrange xfirst xlast xstep)
Make range of num values
* xfirst - first range value
* xlast - last range value (excluded)
* xstep - range step
returns: Range list
## (list-cdrr lst)
Acts like (cdr) but removes all repeating members from beginning
* lst - list to cdrr
returns: list without first n elements
## (list-remove-repeats lst)
Removes repeating in a row members of the list
* lst - lst to remove items
returns: lst without repeating members
## (list-cdrr-prs lst prs)
Acts like (cdr) but removes all repeating members from beginning with precision
* lst - list to cdrr
* prs - presision
returns: list without first n elements
## (list-remove-repeats-prs lst prs)
Removes repeating in a row members of the list with precision
* lst - lst to remove items
* prs - presision
returns: lst without repeating members
## (list-massoc key lst)
Multiple list assoc
* key - key to assoc
* lst - lst to assoc key
returns: list of found items
## (list-flatten lst)
Flatten any list
* lst - list to flatten
returns: One level list
## (list-slice lst firsti lasti)
Slice list between elements
* lst - list to slice
* firsti - start index
* lasti - finish index
returns: Sliced list
## (list-insert-at lst el index)
Insert element at index position
* lst - list to insert element
* el - element to insert
* index - position of element
returns: new list
## (list-dict lst)
Transform list into dict with lists. <br/> Example: <br/> (list-dict ((2 3) (2 4) (2 5) (1 1) (1 2) (1 3))) <br/> ((1 1 2 3) (2 3 4 5))
* lst - list to transform
returns: new list
## (list-swap-values lst fi si)
Swap list elements by indices
* lst - list to swap elements
* fi - first index
* si - second index
returns: Swapped list
## (list-pair-distance lst)
Calculates distance between list members
* lst - list to proceed
returns: list of calculated distace
