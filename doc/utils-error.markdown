# d:/src/AutoCAD-Utils\utils\utils-error.lsp
## (error-try test catch finally
Error trap subroutine <br/> like try-catch-finally <br/> syntax: <br/> (error-try <br/> '(lambda () subroutines to execute) <br/>	'(lambda (ex) subroutines to process errors) <br/>	'(lambda () subroutines to execute anyway))
* test - lambda subr to execute
* catch - lambda subr to process errors
* finally - subroutines to execute anyway
returns: test subr result or catch result
