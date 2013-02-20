# G:\acadext\AutoCAD-Utils\utils\utils-acad.lsp
##(acad-version)
Retrives autocad version as float
* No arguments
returns: Float AutoCAD version number
##(acad-object)
Get AutoCAD.Application object pointer
* No arguments
returns: AutoCAD.Application pointer
##(acad-actdoc)
Get ActiveDocument pointer
* No arguments
returns: ActiveDocument pointer
##(acad-paperspace doc)
Get PaperSpace pointer
* doc - activedocument pointer (auto if nil)
returns: PaperSpace pointer
##(acad-modelspace doc)
Get ModelSpace pointer
* doc - activedocument pointer (auto if nil)
returns: ModelSpace pointer
##(acad-currentspace doc)
Get CurrentSpace pointer
* doc - activedocument pointer (auto if nil)
returns: Current Paper or Model space pointer
##(acad-dump)
Dump vla-object
* No arguments
returns: nil
##(acad-vlasel)
Get vla-object from entsel
* No arguments
returns: vla-object
##(acad-entsel)
Entity selection
* No arguments
returns: entity
##(acad-ent2vla entity)
Get vla-object from entity
* entity - entity to convert
returns: vla-object
##(acad-selset-onscreen etype)
Select objects on screen
* etype - type of entities
returns: VLA Selection Set
##(acad-selset-universal)
Create selection set <br/> with user prompt or selected objects
* No arguments
returns: Selection Set
##(acad-ent-boundingbox entity)
Get entity bounding box coordinates
* entity - entity to get coords (vla or ent)
returns: list of points (min max)
##(acad-selset-boundingbox sel)
Get selection set bounding box
* sel - VLA selection set
returns: list of points (min max)
##(acad-list-boundingbox entlist)
Get entities list bounding box
* entlist - list of entities
returns: list of points (min max)
##(acad-boundingbox-blc clist)
Get the bottom left corner coordinates
* clist - coordinates list
returns: List of coordinates
##(acad-boundingbox-ulc clist)
Get the upper left corner coordinates
* clist - coordinates list
returns: List of coordinates
##(acad-boundingbox-urc clist)
Get the upper right corner coordinates
* clist - coordinates list
returns: List of coordinates
##(acad-boundingbox-brc clist)
Get the bottom right corner coordinates
* clist - coordinates list
returns: List of coordinates
##(acad-ent-get-layer ent
Get entity layer
* ent - ENAME entity
returns: ENAME layer
##(acad-ent-get-color ent
Get entity color
* ent - ENAME entity
returns: INT entity color
##(acad-ent-get-ltype ent
Get entity linetype
* ent - ENAME entity
returns: STR entity linetype
##(acad-ent-get-lweight ent
Get entity lineweight
* ent - ENAME entity
returns: INT entity lineweight
##(acad-objectid vlent)
Get ObjectId inspite of system bit
* No arguments
returns: ObjectID
##(acad-ownerid vlent)
Get OwnerId inspite of system bit
* No arguments
returns: OwnerID
##(acad-objectidtoobject doc id)
Get object by ObjectId
* doc - AutoCAD.Application.Document pointer
* id - objectId
returns: vla-object
