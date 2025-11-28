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



;; Local Variables

(defvar-local scoot-ddl--result-context nil
  "The original describle table result-context of the current buffer.")

(defvar-local scoot-ddl-edit--result-context nil
  "The current raw result-context of the modified object.")


;; DDL

(defun scoot-ddl--on-ddl-change (event)
  "React to a change event from the DDL query block.

Passes the updated DDL to the server to produce an updated table model
reflecting the changes in the DDL statement.

EVENT contains the change data as emitted by the widget event producer."
  (scoot-connection--ddl-to-model scoot-buffer-connection
                                  (plist-get event :data)
                                  (lambda (result-context)
                                    (setq scoot-ddl-edit--result-context result-context))))

(defun scoot-ddl--update (result-context)
  "Update the current ddl buffer with a new DDL result.

RESULT-CONTEXT contains the query statement, ddl data and connection used."
  (let* ((edit-mode-p (eq major-mode 'scoot-ddl-edit-mode))
         (result (plist-get result-context :result))
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
                            :data (if (and edit-mode-p scoot-ddl-edit--result-context)
                                      (plist-get scoot-ddl-edit--result-context :result)
                                      result)
                            :editablep edit-mode-p)
                      (if edit-mode-p
                          (list :type 'query-editor
                                :title "DDL:"
                                :current-sql (alist-get "CREATE TABLE-statement"
                                                        (alist-get 'sql
                                                                   (alist-get 'metadata result)) nil nil #'equal)
                                :on-change #'scoot-ddl--on-ddl-change)
                        (list :type 'ddl-outline
                              :data (alist-get 'metadata result)))))
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
  (let* ((buf-name (scoot-rs--do-generate-buffer-name result-context))
         (buf (get-buffer-create (or buf-name scoot-resultset-buffer-default-name))))
    (with-current-buffer buf
      (scoot-ddl-mode)
      (setq scoot-ddl--result-context result-context)
      (scoot-ddl--update result-context)
      (pop-to-buffer buf))
    buf))

(defun scoot-ddl--open-edit-mode (result-context &optional same-buffer)
  "Opens the Scoot DDL Edit mode for RESULT-CONTEXT.

Optionally re-uses a scoot-ddl-mode buffer if SAME-BUFFER is non-nil."
  (when (and same-buffer (not (eq major-mode 'scoot-ddl-mode)))
    (error "Current buffer is not a scoot-ddl buffer"))
  (let* ((buf-name (scoot-rs--do-generate-buffer-name result-context))
         (buf (if same-buffer (current-buffer)
                (get-buffer-create (or buf-name scoot-resultset-buffer-default-name)))))
    (with-current-buffer buf
      (when same-buffer
        (rename-buffer buf-name))
      (scoot-ddl-edit-mode)
      (setq scoot-ddl--result-context result-context)
      (scoot-ddl--update result-context)
      (pop-to-buffer buf))
    buf))



;; Scoot DDL Mode - scoot-ddl-mode

(defun scoot-ddl-edit-table ()
  "Enter DDL Edit Mode based on a current DDL Mode buffer."
  (interactive)
  (scoot-ddl--open-edit-mode scoot-ddl--result-context t))

(defvar scoot-ddl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scoot-buffer-mode-map)
    (define-key map (kbd "e") #'scoot-ddl-edit-table)
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

(define-derived-mode scoot-ddl-edit-mode scoot-buffer-mode "Scoot Edit DDL"
  "Major mode for displaying and interacting with table definitions."
  (setq-local scoot-local--table-name-resolvers '(scoot-rs--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local scoot-xref-action-fn 'scoot-rs--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-rs--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-rs--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t)

  (scoot-table--ensure-row-mark-table))



(provide 'scoot-ddl)



;;; scoot-ddl.el ends here
