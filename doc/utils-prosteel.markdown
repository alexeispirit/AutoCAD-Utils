# d:/src/AutoCAD-Utils\utils\utils-prosteel.lsp
## (acad-dicts)
AutoCAD ActiveDocument dictionaries collection
* actdoc - AutoCAD ActiveDocument pointer
returns: vlax Dictionaries collection
## (prosteel-dicts)
Prosteel dictionaries names
* No arguments
returns: List of Bentley Prosteel/Structural dictionaries
## (prosteel-check)
Check is there any prosteel dictionaries in drawing database
* No arguments
returns: T or nil
## (prosteel-remove-dicts-vl)
Remove all prosteel dictionaries using vla-delete (proxy-objects)
* No arguments
returns: nil
## (prosteel-remove-dicts-ent)
Remove all prosteel dictionaries using dictremove (proxy-objects)
* No arguments
returns: nil
