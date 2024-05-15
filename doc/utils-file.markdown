# d:/src/AutoCAD-Utils\utils\utils-file.lsp
## (file-info)
Retrives file information
* No arguments
returns: list of FILEINFO structure <br/> (("FILE" ...) <br/> ("PATH" ...) <br/> ("DIR" ...))
## (file-from-projectwise fileinfo)
Checks whether file in PW or not
* fileinfo - list of FILEINFO structure
returns: T or nil
## (file-read-config file)
Reads config file and returns list of values
* file - path to file
returns: list of read values
## (file-read-value key config)
Reads value from (key . value) config file
* key - key to read
* config - config list to read from
returns: matching value
## (file-qrcode-generate string)
Generates QRcode from string
* string - string to encode
returns: path to QRcode image
## file-join-path (lst)
Builds path to file from list
* lst - file path list
returns: string path to file
## file-load-directory (lst)
Loads all files from directory
* dir - directory to load
returns: void
## (file-netload filename)
Load .Net assembly into autocad
* filename - path to dll file
returns: T or nil
