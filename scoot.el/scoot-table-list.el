;;; scoot-table-list.el --- summary -*- lexical-binding: t -*-
;; -*- read-symbol-shorthands: (("plist-get-in" . "scoot--plist-get-in")) -*-

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

;;; Code:

(require 'scoot-buffer)
(require 'scoot-connection)
(require 'scoot-resultset)
(require 'scoot-table)
(require 'scoot-xref)


;; Local Variables

(defvar scoot-table-list-include-row-count nil
  "Non-nil values enable row count per table in table list mode.")



;; Table list

(defun scoot-tbll--update (result-context)
  (let ((result (plist-get result-context :result)))
    (setq scoot-buffer-connection (plist-get result-context :connection)
          scoot-rs--result-type 'table
          scoot-buffer-sections
          (list 'connection-header
                (list :type 'object-type-header
                      :data (list :object-type 'tables
                                  :object-name "Table List"))
                (list :type 'data-table
                      :data result)
                (list :type 'ddl-outline
                      :data (alist-get 'metadata result))))
    (scoot-buffer-refresh)))

(defun scoot-tbll--refetch ()
  "Refetch the table list."
  (scoot--list-objects 'tables
                       scoot-buffer-connection
                       (lambda (table-result)
                         (scoot-tbll--update (scoot-tbll--table-list-to-result-context
                                              table-result
                                              scoot-buffer-connection)))
                       (list :include-row-count scoot-table-list-include-row-count))
  "Fetching table list...")

(defun scoot-tbll--table-list-to-result-context (table-list-result connection)
  "Transforms a TABLE-LIST-RESULT to RESULT-CONTEXT.

CONNECTION is the database connection that was used to retrieve the table list."
  (let ((columns (seq-filter #'identity
                             (mapcar
                              (lambda (prop-name)
                                (pcase prop-name
                                  ("name" '((name . "Table Name")
                                            (type . "OBJECT-NAME")))
                                  ("row_count" '((name . "Row Count")
                                                 (type . "INTEGER")))))
                              (seq-into (alist-get 'properties table-list-result) 'list)))))
    (list :type 'objects
          :object-type 'tables
          :result
          `((columns . ,(seq-into
                         (mapcar (lambda (col) (alist-get 'name col)) columns)
                         'vector))
            (rows . ,(seq-into
                      (seq-into
                       (alist-get 'tables table-list-result)
                       'vector)
                      'vector))
            (metadata . ((columns . ,(seq-into columns 'vector)))))
          :connection connection)))



;; Entry points

(cl-defun scoot--open-table-list (result-context &key force-new-buffer)
  "Open a Scoot ResultSet Buffer with a result described by RESULT-CONTEXT.

RESULT-CONTEXT is expected to be a plist, with the following possible keys:
:result - contains the headers, rows and metadata of the result.
:connection - describes the connection used to retrieve this result.
:type - the type of result (query/objects/object).
:buffer-name - Optional name/identity of the result buffer.

Additional keys for type query:
:statement - The SQL statement that produced the result.

Additional keys for type object/objects:
:object-type - The type of object (table/schema/database).

Additional keys for type object:
:object-name - The name of the object described.

Optional keys:
FORCE-NEW-BUFFER - Force the creation of a new buffer if a buffer with
                   the target name already exists by killing the old
                   buffer first."
  (let* ((buf-name (scoot-rs--do-generate-buffer-name result-context))
         (buf (progn
                (when-let ((force-new-buffer force-new-buffer)
                           (existing-buf (get-buffer buf-name)))
                  (kill-buffer existing-buf))
                (get-buffer-create (or buf-name scoot-resultset-buffer-default-name)))))
    (with-current-buffer buf
      (scoot-table-list-mode)
      (scoot-tbll--update result-context)
      (pop-to-buffer buf))
    buf))



;; Column toggle functions

(defun scoot--toggle-table-list-row-count ()
  (interactive)
  (setq scoot-table-list-include-row-count (null scoot-table-list-include-row-count))
  (scoot-tbll--refetch))



;; Resolvers

(defun scoot-tbll--table-at-point (&allow-other-keys)
  "Return table name at point if point is in a table name cell."
  (when-let* ((cell (scoot-table--cell-at-point))
              (column (plist-get cell :column)))
    (when (equal (alist-get 'name column) "Table Name")
      (plist-get cell :value))))



;; Scoot Table List Mode - scoot-table-list-mode

(defvar scoot-table-list-mode-map
  (let ((map (make-sparse-keymap))
        (scoot-tbll-info-toggle-keymap (make-keymap)))
    (set-keymap-parent map scoot-buffer-mode-map)
    (define-key scoot-tbll-info-toggle-keymap (kbd "c") #'scoot--toggle-table-list-row-count)
    (define-key map (kbd "i") scoot-tbll-info-toggle-keymap)
    map)
  "Keymap for `scoot-table-list-mode`.")

(define-derived-mode scoot-table-list-mode scoot-buffer-mode "Scoot Table List"
  "Major mode for displaying and interacting with table definitions."
  (setq-local scoot-local--table-name-resolvers '(scoot-tbll--table-at-point
                                                  scoot-rs--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local scoot-xref-action-fn 'scoot-rs--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-rs--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-rs--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t))



(provide 'scoot-table-list)

;;; scoot-table-list.el ends here
