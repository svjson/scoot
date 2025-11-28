;;; scoot-ddl.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2025 Sven Johansson

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

;; Contains the scoot-ddl-mode used for interactively viewing, creating or
;; modifying CREATE statements or the dialect equivalent.

;;; Code:

(require 'scoot-buffer)
(require 'scoot-resultset)



;; Forward declarations

(declare-function scoot-describe-table "scoot")
(declare-function scoot-list-databases "scoot")
(declare-function scoot-list-schemas "scoot")
(declare-function scoot-list-tables "scoot")



;; DDL

(defun scoot-ddl--update (result-context)
  "Update the current ddl buffer with a new DDL result.

RESULT-CONTEXT contains the query statement, ddl data and connection used."
  (let* ((result (plist-get result-context :result))
         (stmt (or (alist-get 'stmt (alist-get 'metadata result))
                   (plist-get result-context :statement)))
         (object-type (plist-get result-context :object-type))
         (object-name (plist-get result-context :object-name))
         (type (plist-get result-context :type)))
    (setq-local scoot-buffer-connection (plist-get result-context :connection)
                scoot-rs--result-type type
                scoot-rs--current-sql-statement stmt
                scoot-buffer-sections
                (list 'connection-header
                      (list :type 'object-type-header
                            :data (list :object-type object-type
                                        :object-name object-name))
                      (list :type 'data-table
                            :data result
                            :editablep nil)
                      (list :type 'ddl-outline
                            :data (alist-get 'metadata result))))

    (scoot-buffer-refresh)))




;; Scoot DDL Entry Points

(defun scoot-ddl--open-table (result-context)
  "Open a Scoot DDL Buffer with a table described by RESULT-CONTEXT.

RESULT-CONTEXT is expected to be a plist, with the following possible keys:
:result - contains the headers, rows and metadata of the table result.
:connection - describes the connection used to retrieve the table.
:type - the type of result (query/objects/object).
:buffer-name - Optional name/identity of the result buffer.

Additional keys for type object/objects:
:object-type - The type of object (table/schema/database).

Additional keys for type object:
:object-name - The name of the object/table described."
  (let ((result (plist-get result-context :result))
        (connection (plist-get result-context :connection))
        (stmt (plist-get result-context :statement))
        (type (plist-get result-context :type))
        (object-type (plist-get result-context :object-type))
        (object-name (plist-get result-context :object-name))
        (buf-name-override (plist-get result-context :buffer-name)))
    (let* ((buf-name (cond ((null scoot-generate-resultset-buffer-name-function) scoot-resultset-buffer-default-name)
                           ((symbolp scoot-generate-resultset-buffer-name-function)
                            (let ((gen-fn (symbol-function scoot-generate-resultset-buffer-name-function)))
                              (if (functionp gen-fn)
                                  (funcall gen-fn connection result stmt type object-type object-name buf-name-override)
                                scoot-resultset-buffer-default-name)))
                           (t scoot-resultset-buffer-default-name)))
           (buf (get-buffer-create (or buf-name scoot-resultset-buffer-default-name))))
      (with-current-buffer buf
        (scoot-ddl-mode)
        (scoot-ddl--update result-context)
        (pop-to-buffer buf))
      buf)))




;; Scoot DDL Mode - scoot-ddl-mode

(defun scoot-edit-ddl ()
  "Enter DDL Edit Mode."
  (interactive)
  (scoot-ddl-edit-mode 1))

(defvar scoot-ddl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scoot-buffer-mode-map)
    (define-key map (kbd "e") #'scoot-edit-ddl)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    map)
  "Keymap for `scoot-ddl-mode`.")

(define-derived-mode scoot-ddl-mode scoot-buffer-mode "Scoot DDL"
  "Major mode for displaying and interacting with table definitions."
  (setq-local scoot-local--table-name-resolvers '(scoot-rs--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local scoot-xref-action-fn 'scoot-rs--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-rs--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-rs--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t)

  (scoot-table--ensure-row-mark-table))



;; Scoot DDL Edit Mode - scoot-ddl-edit-mode

(defvar scoot-ddl-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scoot-buffer-mode-map)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    map)
  "Keymap for `scoot-ddl-mode`.")

(define-minor-mode scoot-ddl-edit-mode
  "Minor mode overlay for editing table definitions."
  :lighter " Edit"
  :keymap scoot-ddl-edit-mode-map
  (setq-local scoot-local--table-name-resolvers '(scoot-rs--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local scoot-xref-action-fn 'scoot-rs--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-rs--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-rs--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t)

  (scoot-table--ensure-row-mark-table)

  (setq-local scoot-buffer-sections
              (mapcar (lambda (section)
                        (pcase (plist-get section :type)
                          ('data-table (progn (setf (plist-get section :editablep) t)
                                              section))
                          ('ddl-outline (list :type 'query-editor
                                              :title "DDL:"
                                              :current-sql (cdr (car (alist-get 'sql (plist-get section :data))))))
                          (_ section)))
                       scoot-buffer-sections))

  (scoot-buffer-refresh))



(provide 'scoot-ddl)



;;; scoot-ddl.el ends here
