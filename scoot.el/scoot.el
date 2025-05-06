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
(require 'scoot-common)

(defgroup scoot nil
  "Scoot-modes for SQL interaction via SQL scratch buffers and result set buffers."
  :group 'tools)

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

(defun scoot-describe-table (&optional table-name connection-name)
  "Describe a table as defined by arguments or selected interactively.

TABLE-NAME and CONNECTION-NAME are optional, and will be attempted to be
resolved using the context of the function call, if called interactively."
  (interactive)
  (let* ((conn-name (or connection-name
                        (scoot--interactive-resolve-connection-name)))
         (conn (gethash conn-name scoot-connections))
         (tbl (or table-name
                  (scoot--try-resolvers
                   'scoot-local--table-name-resolvers
                   'scoot-default--table-name-resolvers
                   (list :connection-name conn-name
                         :interactive t)))))
    (scoot-connection--describe-table conn
                                      tbl
                                      #'scoot-result--open-result-buffer)))

(provide 'scoot)

;;; scoot.el ends here
