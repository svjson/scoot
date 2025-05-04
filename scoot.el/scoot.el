;;; scoot.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson (johansson.sven@gmail.com)
;; Maintainer: Sven Johansson (johansson.sven@gmail.com)
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (request "20250219") (sql "3.0))
;; Homepage: https://www.github.com/svjson/scoot
;; Keywords: sql, database, sql-client, tools, convenience

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

;; Scoot is a SQL client for Emacs that interacts with a persistent backend server.
;; It supports scratch buffers, result metadata, connection management, and editing.

;;; Code:

(require 'request)
(require 'scoot-server)
(require 'scoot-connection)
(require 'scoot-result)
(require 'scoot-scratch)

(defgroup scoot nil
  "Scoot-modes for SQL interaction via SQL scratch buffers and result set buffers."
  :group 'tools)

(defun with-contextual-connection (callback)
  "Execute a callback/function after ensuring a connection.

CALLBACK is the function to be called after verifying the connection."
  (let* ((ctx (scoot--resolve-context-at-point))
         (ctx-props (cdr ctx))
         (connection-name (gethash "connection-name"
                                   ctx-props
                                   scoot-server-default-connection-name))
         (connection-string (gethash "connection-string"
                                     ctx-props
                                     nil)))
    (scoot--ensure-connection
     (lambda ()
       (funcall callback connection-name))
     connection-name
     connection-string)))

(defun scoot--list-objects (object-type title)
  "List objects visible to the user of the current connection.

OBJECT-TYPE is the type of the object to list.
TITLE is the resultset header to be used."
  (interactive)
  (with-contextual-connection
   (lambda (connection-name)
     (request
       (format "%s/%s"
               (scoot-server-base-connection-url connection-name)
               object-type)
       :type "GET"
       :headers scoot--json-headers
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (scoot--open-result-buffer
                    (list :type 'objects
                          :object-type object-type
                          :result
                          `((columns . [,title])
                            (rows . ,(mapcar #'vector
                                             (seq-into
                                              (alist-get object-type data)
                                              'vector)))
                            (metadata . ((columns . [((name . ,title)
                                                      (type . "OBJECT-NAME"))]))))
                          :connection connection-name))))
       :error (scoot-connection--error-handler :op 'scoot-list-objects)))))

(defun scoot-list-databases ()
  "List databases visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'databases "Database Name"))

(defun scoot-list-schemas ()
  "List schemas visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'schemas "Schema Name"))

(defun scoot-list-tables ()
  "List tables visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'tables "Table Name"))

(defun scoot-describe-table (table-name)
  "Describe a database table.

If TABLE-NAME is provided, this will be used as table name."
  (interactive
   (list
    (cond
     ((use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end)))
     ((thing-at-point 'symbol t))
     (t
      (read-string "Table name: ")))))
  (with-contextual-connection
   (lambda (connection-name)
     (scoot-ensure-server)
     (request
       (format "%s/tables/%s"
               (scoot-server-base-connection-url connection-name)
               table-name)
       :type "GET"
       :headers scoot--json-headers
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let ((columns '(name type nullable primary_key default)))
                     (scoot--open-result-buffer
                      (list :type 'object
                            :object-type 'table
                            :object-name (alist-get 'name data)
                            :connection connection-name
                            :result `((columns . ,columns)
                                      (rows . ,(mapcar (lambda (entry)
                                                         (mapcar (lambda (col-name)
                                                                   (alist-get col-name entry))
                                                                 columns))
                                                       (alist-get 'columns data)))
                                      (metadata . ((columns . [((name . "Name")
                                                                (type . "OBJECT-NAME"))
                                                               ((name . "Type")
                                                                (type . "DATA-TYPE"))
                                                               ((name . "Nullable")
                                                                (type . "BOOLEAN"))
                                                               ((name . "Primary Key")
                                                                (type . "BOOLEAN"))
                                                               ((name . "Default")
                                                                (type . "self(Type)"))])
                                                   (sql . (("CREATE TABLE-statement" . ,(alist-get 'create_stmt data))))))))))))
       :error (scoot-connection--error-handler :op 'scoot-describe-table)))))




(provide 'scoot)

;;; scoot.el ends here
