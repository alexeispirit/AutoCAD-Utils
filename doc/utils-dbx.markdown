# D:\src\AutoCAD-Utils\utils\utils-dbx.lsp
## (dbx-object)
Get ObjectDBX object point
* No arguments
returns: ObjectDBX.AxDbDocument pointer
## (dbx-copy)
Copy all object from dbx document to active document
* dbx - ObjectDBX pointer
* src - Source collection in DBX document
* dst - Destionation collection
returns: None
## (dbx-import)
Import textstyle, dimstyles, linetypes and layers from DBX to active document
* filename - DWG filename to open as DBX
returns: None
