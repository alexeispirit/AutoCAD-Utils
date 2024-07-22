# D:\src\AutoCAD-Utils\utils\utils-sset.lsp
## (sset-join)
Join 2 selection sets
* ss1 - Selection set
* ss2 - Selection set
returns: New Selection set
## (sset-all-database entity-filter)
Select all entities in database
* entity-filter - string for entity filter. "*" for all
returns: New Selection set
## (sset-all-current-space entity-filter)
Select all entities in current space
* entity-filter - string for entity filter. "*" for all
returns: New Selection set
## (sset-all-by-layer entity-filter layer)
Select all entities in current space and layer name
* entity-filter - string for entity filter. "*" for all
* layer - string for layer name
returns: New Selection set
## (sset-by-user entity-filter)
Select entities by user
* entity-filter - string for entity filter. "*" for all
returns: New Selection set
