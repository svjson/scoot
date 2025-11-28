;;; scoot-nexartrade-fixtures.el --- summary -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)



(cl-defun nexartrade-fake-connection (&key context name database host port dialect driver)
  "Create a fake connection model.

Override any property by providing CONTEXT, NAME, DATABASE, HOST,
PORT, DIALECT and/or DRIVER."
  (list :context (or context "nexartrade")
        :name (or name "nexartrade")
        :database (or database "nexartrade")
        :host (or host "staging.nexartrade.local")
        :port (or port "1433")
        :dialect (or dialect "mssql")
        :driver (or driver "pysql+odbc")))

(cl-defun nexartrade-result-context--table-users (&key connection)
  "Create a result-context describing nexartrade table `users`.

CONNECTION may be provided to override the default nexartrade
connection."
  (list :result '((columns . ("name" "type" "nullable" "primary_key" "default"))
                  (rows . (("id" "int" :json-false t nil)
                           ("username" "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS" t :json-false nil)
                           ("email" "varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS" t :json-false nil)
                           ("password_hash" "varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS" t :json-false nil)
                           ("created_at" "datetimeoffset" t :json-false nil) ("last_login" "datetimeoffset" t :json-false nil)
                           ("is_active" "bit" t :json-false nil)))
                  (metadata . ((columns . [((name . "Name") (type . "OBJECT-NAME"))
                                           ((name . "Type") (type . "DATA-TYPE"))
                                           ((name . "Nullable") (type . "BOOLEAN"))
                                           ((name . "Primary Key") (type . "BOOLEAN"))
                                           ((name . "Default") (type . "self(Type)"))])
                               (sql . (("CREATE TABLE-statement" . "\nCREATE TABLE users (\n	id INTEGER NOT NULL IDENTITY(1,1), \n	username VARCHAR(50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL, \n	email VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL, \n	password_hash VARCHAR(255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL, \n	created_at DATETIMEOFFSET NULL, \n	last_login DATETIMEOFFSET NULL, \n	is_active BIT NULL, \n	CONSTRAINT [PK__users__3213E83FAAF8442F] PRIMARY KEY CLUSTERED (id)\n)\n\n"))))))
        :connection (or connection (nexartrade-fake-connection))
        :type 'object
        :object-type 'table
        :object-name "users"))




(provide 'scoot-nexartrade-fixtures)

;;; scoot-nexartrade-fixtures.el ends here
