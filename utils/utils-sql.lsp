;;; <LISPDOC>
;;; <SUBR>(sql-mssql-connection-string server db login pwd)</SUBR>
;;; <DESC>Generates MSSQL DB connection string</DESC>
;;; <ARG>server - server hostname to connect</ARG>
;;; <ARG>db - database to connect</ARG>
;;; <ARG>login - loginname</ARG>
;;; <ARG>pwd - password</ARG>
;;; <RET>MSSQL Server connection string</RET>
;;; </LISPDOC>
(defun sql-mssql-connection-string (server, db, login, pwd) 
  (strcat "Provider=SQLOLEDB;Driver={SQL Server};Server=" server ";Database=" db 
          ";UID=" login ";PWD=" pwd))