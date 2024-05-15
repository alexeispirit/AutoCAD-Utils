# d:/src/AutoCAD-Utils\utils\utils-matrix.lsp
## (matrix-remove-rowcol)
Removes given row and column from given matrix
* mat - matrix
* size - size of matrix
* row - index of row to remove
* col - index of column to remove
returns: New matrix without give row and column
## (matrix-determinant)
Calculates determinant of square matrix
* mat - square matrix
* size - size of matrix
returns: Matrix determinant
## (matrix-replace-col)
Replaces column of matrix with given values
* mat - square matrix
* size - size of matrix
* col - index of column
* values - list of values
returns: Matrix with replaced column
## (matrix-from-list)
Generates square matrix from given list
* lst - list of values
* size - size of new matrix
returns: Matrix
## (matrix-to-list)
Generate square matrix from given list
* mat - matrix
returns: Plain list
## (matrix-multiply-matrix)
Performs matrix on matrix multiplication
* a - matrix 1
* b - matrix 2
returns: Result of matrix multiplication
## (matrix-multiply-scalar)
Performs matrix on scalar multiplication
* mat - matrix
* k - number
returns: Result of matrix multiplication
## (matrix-transpose)
Transposes matrix
* mat - matrix
returns: Transposed matrix
## (matrix-complement)
Calculates matrix complement for given element
* mat - matrix
* size - size of matrix
* row - index of row
* col - index of column
returns: Matrix complement for element
## (matrix-union)
Calculates union matrix
* mat - matrix
* size - size of matrix
returns: Union matrix
## (matrix-inverse)
Calculates inverse matrix
* mat - matrix
* size - size of matrix
returns: Inversed matrix
