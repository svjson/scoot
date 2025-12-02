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


;; Fixture functions

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

(cl-defun scoot-test--result-data (&key data columns rows)
  "Trim result DATA according to COLUMNS and ROWS."
  (let* ((columns (when columns (seq-into columns 'list)))
         (col-names (seq-into (alist-get 'columns data) 'list))
         (data-rows (seq-into (alist-get 'rows data) 'list))
         (column-metadata (seq-into (alist-get 'columns (alist-get 'metadata data)) 'list))
         (col-indices (mapcar (lambda (col-name)
                                (cl-position col-name col-names :test #'equal))
                              (if (null columns) col-names columns))))
    `((columns . ,(seq-into (mapcar (lambda (index)
                                      (nth index col-names))
                                    col-indices)
                            'vector))
      (rows . ,(seq-into (mapcar (lambda (row)
                                   (seq-into (mapcar (lambda (index)
                                                       (aref row index))
                                                     col-indices)
                                             'vector))
                                 (cond
                                  ((null rows)
                                   data-rows)

                                  ((consp rows)
                                   (cl-loop for index from (car rows) to (cdr rows)
                                            collect (nth index data-rows)))

                                  ((sequencep rows)
                                   (mapcar (lambda (index)
                                             (nth index data-rows))
                                           rows))))
                         'vector))
      (metadata . ((columns . ,(seq-into
                                 (mapcar (lambda  (index)
                                           (nth index column-metadata))
                                         col-indices)
                                 'vector)))))))

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


;; Data

(defvar nexartrade-table--users--result-data
  '((columns
     . ["id" "username" "email" "password_hash" "created_at" "last_login"
        "is_active"])
    (rows
     . [[1 "tom_atojuicesson" "tom.atojuicesson@example.com"
           "3046dc1c9cd8656bcc71d3b78dd27e30f719a0f71f7aeb531e2d656328eb65c7"
           "2023-02-08T07:48:04+00:00" "2024-09-26T17:52:39+00:00"
           :json-false]
        [2 "ben_rangel" "ben.rangel@example.com"
           "28a85c69981b4d83c15829f999b27b89bb5016736f71498c51dbeb229d9b9a2f"
           "2023-04-28T12:45:21+00:00" "2024-05-03T11:29:59+00:00"
           :json-false]
        [3 "elle93" "ella.vator@example.com"
           "6c6637f8a1b9d6844ec447e48812cf3c2fccd6baf86752644ac2f2632149d948"
           "2023-11-28T18:31:59+00:00" "2024-11-18T17:38:17+00:00" t]
        [4 "hugh_jass" "hugh.jass@example.com"
           "2f91adbbfc71ba05bd467721aa21c7c2adb76b64656cbc995037872a93072c53"
           "2023-11-04T01:26:32+00:00" "2024-03-02T07:42:07+00:00"
           :json-false]
        [5 "max_power" "max.power@example.com"
           "02ce78bf481da90435320ed65b3ead8f74fa8dfe8bd5ab31dfdb59a46c8577d1"
           "2023-03-21T00:24:41+00:00" "2024-04-28T06:24:32+00:00" t]
        [6 "sue_permann" "sue.permann@example.com"
           "db38d915621a01cb8ad7987bb4c01d5c94ab32b000215e46259fccaa0c5e7a2a"
           "2023-01-30T10:11:13+00:00" "2024-03-23T08:23:49+00:00"
           :json-false]
        [7 "barb_dwyer" "barb.dwyer@example.com"
           "3120c43ba79c2399e092fae856ac3689ea2ae3b1e7c4af7b7ad934cb8727f4d3"
           "2023-11-13T03:46:01+00:00" "2024-07-05T04:45:59+00:00" t]
        [8 "melodies" "mel.n.collie@example.com"
           "d0856e825cec051407edaf97217cc252745153fc007bbdd7f16722248d73974d"
           "2023-04-05T23:15:39+00:00" "2024-02-29T20:52:32+00:00" t]
        [9 "skywalker_77" "luke_warm@example.com"
           "abd5e566e085a363e3caec0e5330777f554fdfc87deb7e2f7931770cdb5920b3"
           "2023-07-20T12:58:00+00:00" "2025-03-12T02:11:58+00:00" t]
        [10 "deedeesalmon" "dee.zaster@example.com"
            "dfd2e66828d3306a2fbf5b4a9c332bb722abb9981f52d1804e943089c49eb126"
            "2023-01-17T20:54:53+00:00" "2025-01-03T23:10:06+00:00" t]])
    (metadata
     (columns
      . [((name . "id") (type . "INTEGER")
          (typespec (type . "INTEGER") (bits . 64) (signed . t))
          (native_type . "int") (nullable . :json-false)
          (primary_key . t) (default) (table . "users") (column . "id")
          (constraints . []))
         ((name . "username") (type . "STRING")
          (typespec (type . "STRING") (max-len . 50) (encoding . "utf-8")
                    (collation (locale . "Latin1_General_CP1")
                               (case-sensitive . :json-false)
                               (accent-sensitive . t))
                    (lob . :json-false))
          (native_type
           . "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS")
          (nullable . t) (primary_key . :json-false) (default)
          (table . "users") (column . "username") (constraints . []))
         ((name . "email") (type . "STRING")
          (typespec (type . "STRING") (max-len . 100)
                    (encoding . "utf-8")
                    (collation (locale . "Latin1_General_CP1")
                               (case-sensitive . :json-false)
                               (accent-sensitive . t))
                    (lob . :json-false))
          (native_type
           . "varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS")
          (nullable . t) (primary_key . :json-false) (default)
          (table . "users") (column . "email") (constraints . []))
         ((name . "password_hash") (type . "STRING")
          (typespec (type . "STRING") (max-len . 255)
                    (encoding . "utf-8")
                    (collation (locale . "Latin1_General_CP1")
                               (case-sensitive . :json-false)
                               (accent-sensitive . t))
                    (lob . :json-false))
          (native_type
           . "varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS")
          (nullable . t) (primary_key . :json-false) (default)
          (table . "users") (column . "password_hash") (constraints . []))
         ((name . "created_at") (type . "TEMPORAL")
          (typespec (type . "TEMPORAL")
                    (date (min . [1 1 1]) (max . [9999 12 31])
                          (calendar . "gregorian"))
                    (time (clock . "24") (fsp) (precision) (scale) (tz)))
          (native_type . "datetimeoffset") (nullable . t)
          (primary_key . :json-false) (default) (table . "users")
          (column . "created_at") (constraints . []))
         ((name . "last_login") (type . "TEMPORAL")
          (typespec (type . "TEMPORAL")
                    (date (min . [1 1 1]) (max . [9999 12 31])
                          (calendar . "gregorian"))
                    (time (clock . "24") (fsp) (precision) (scale) (tz)))
          (native_type . "datetimeoffset") (nullable . t)
          (primary_key . :json-false) (default) (table . "users")
          (column . "last_login") (constraints . []))
         ((name . "is_active") (type . "INTEGER")
          (typespec (type . "INTEGER") (bits . 1) (signed . :json-false))
          (native_type . "bit") (nullable . t)
          (primary_key . :json-false) (default) (table . "users")
          (column . "is_active") (constraints . []))])))
  )



(provide 'scoot-nexartrade-fixtures)

;;; scoot-nexartrade-fixtures.el ends here
