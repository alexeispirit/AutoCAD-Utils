(defun sql-mssql-connection-string (server, db, login, pwd)
  (strcat "Provider=SQLOLEDB;Driver={SQL Server};Server=" server ";Database=" db ";UID=" login ";PWD=" pwd))