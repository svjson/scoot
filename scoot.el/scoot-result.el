;;; scoot-result.el --- summary -*- lexical-binding: t -*-

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

;; Contains the result rendering and display mode for Scoot.  It defines
;; `scoot-result-mode`, a read-only major mode for displaying SQL query results
;; in a structured, grid-like format.
;;
;; Result sets are formatted as ASCII/Unicode tables with alignment, type-aware
;; formatting, and optional metadata indicators such as primary key icons.
;;
;; This module is designed to integrate with other Scoot modules such as
;; connection handling and SQL scratch buffers, and supports visual presentation
;; features including header labeling, cell styling by type, and query previews.
;;
;; Faces, keybindings, and model logic are all defined here, with support
;; for future interactive extensions.

;;; Code:

(require 'cl-lib)
(require 'outline)
(require 'scoot-server)
(require 'scoot-connection)

(defcustom scoot-primary-key-icon "🔑"
  "Icon used to indicate that a column has a primary key constraint."
  :type 'string
  :group 'scoot)

(defcustom scoot-foreign-key-icon "🗝️"
  "Icon used to indicate that a column has a foreign key constraint."
  :type 'string
  :group 'scoot)

(defvar-local scoot-result--original-sql-statement nil
  "The original SQL statement that the current buffer is based on.")

(defvar-local scoot-result--current-sql-statement nil
  "The current SQL statement that the current buffer is based on.")

(defvar-local scoot-result--result-connection-name nil
  "The server connection name used by the current Scoot Result Buffer.")

(defvar-local scoot-result--result-type nil
  "The type of result displayed in the current buffer.

Valid values are:
query
objects
object")

(defvar-local scoot-result--result-object-type nil
  "The type of object listed in the current buffer.
Only used/valid when `scoot-result--result-type` is `objects`.

Valid values are:
database
schema
table")

(defvar-local scoot-result--result-object-name nil
  "The name of the object(table/schema/database) described in the buffer.")

(defvar-local scoot-result--result-data nil
  "The result data model for the current Scoot Result buffer.")

(defvar-local scoot-result--result-model nil
  "The model backing the visual representation of the data set.")

(defvar-local scoot-result--outline-sections 0
  "The number of foldable headings currently in the buffer.
Used to enable/disable `outline-minor-mode`.")

(defface scoot-label-face
  '((t :inherit font-lock-function-name-face))
  "Face used for result headers."
  :group 'scoot)

(defface scoot-header-face
  '((t :inherit bold))
  "Face used for result headers."
  :group 'scoot)

(defface scoot-table-face
  '((t :inherit shadow))
  "Face used for resultset table borders."
  :group 'scoot)

(defface scoot-outline-header-face
  '((t :inherit outline-1))
  "Face used for foldable outline headers."
  :group 'scoot)

(defface scoot-query-block-face
  '((t :inherit highlight :background "#222222" :extend nil))
  "Face used fo query blocks."
  :group 'scoot)

(defface scoot-cell-null-face
  '((t :inherit font-lock-comment-face))
  "Face used for null values in result set cells."
  :group 'scoot)

(defface scoot-cell-generic-face
  '((t :inherit default))
  "Face used for default-formatted value type cells."
  :group 'scoot)

(defface scoot-cell-string-face
  '((t :inherit font-lock-string-face))
  "Face used for string values in result set cells."
  :group 'scoot)

(defface scoot-cell-number-face
  '((t :inherit font-lock-number-face))
  "Face used for numeric values in result set cells."
  :group 'scoot)

(defface scoot-cell-boolean-true-face
  '((t :inherit font-lock-keyword-face))
  "Face used for string values in result set cells."
  :group 'scoot)

(defface scoot-cell-boolean-false-face
  '((t :inherit font-lock-operator-face))
  "Face used for string values in result set cells."
  :group 'scoot)

(declare-function scoot-describe-table "scoot")
(declare-function scoot-list-databases "scoot")
(declare-function scoot-list-schemas "scoot")
(declare-function scoot-list-tables "scoot")

(defun scoot--insert-faced (text face)
  "Insert TEXT with FACE applied as a text property."
  (let ((beg (point)))
    (insert text)
    (put-text-property beg (point) 'face face)))

(defun scoot--wrap-string (str width)
  "Wrap STR into lines no longer than WIDTH using word boundaries."
  (let ((words (split-string str " "))
        (lines '())
        (current ""))
    (dolist (word words)
      (if (> (+ (length current) (length word) 1) width)
          (progn
            (push current lines)
            (setq current word))
        (setq current (if (string-empty-p current)
                          word
                        (concat current " " word)))))
    (when (not (string-empty-p current))
      (push current lines))
    (nreverse lines)))

(defun scoot--insert-query-box (query)
  "Insert QUERY string with wrapping and box-like styling."
  (let* ((width (window-body-width))
         (wrapped-lines (scoot--wrap-string query width)))
    (dolist (line wrapped-lines)
      (let ((beg (point)))
        (insert line)
        (insert "\n")
        (put-text-property beg (point) 'face 'scoot-query-block-face)))))

(defun scoot--value-to-string (val &rest _)
  "Stringify a value, VAL, for display in a result set table."
  (if (null val)
      "NULL"
    (format "%s" val)))

(defvar scoot-formatter-header
  (list :align 'left
        :format-value (lambda (name metadata)
                        (format "%s%s"
                                (if (eq t (alist-get 'primary_key metadata))
                                    scoot-primary-key-icon
                                  "")
                                name))
        :output-cell (lambda (_ formatted-value)
                        (scoot--insert-faced formatted-value 'scoot-header-face))))

(defvar scoot-formatter-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-string-face))))

(defvar scoot-formatter-number
  (list :align 'right
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-number-face))))

(defvar scoot-formatter-boolean
  (list :align 'right
        :format-value (lambda (val &rest _)
                        (if (eq val t) "true" "false"))
        :output-cell
        (lambda (val formatted-value)
          (scoot--insert-faced formatted-value
                               (if (eq val t)
                                   'scoot-cell-boolean-true-face
                                 'scoot-cell-boolean-false-face)))))

(defvar scoot-formatter-raw-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-generic-face))))

(defvar scoot-formatter-null
  (list :align 'right
        :format-value (lambda (_) "NULL")
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-null-face))))

(defun scoot--resolve-formatter-from-column-metadata (column)
  "Resolve cell formatter based on COLUMN type."
  (let ((column-type (alist-get 'type column)))
    (cond
     ((string-prefix-p "NVARCHAR" column-type) scoot-formatter-string)
     ((string-equal "OBJECT-NAME" column-type) scoot-formatter-string)
     ((string-equal "INTEGER" column-type) scoot-formatter-number)
     ((string-equal "BOOLEAN" column-type) scoot-formatter-boolean)
     (t scoot-formatter-raw-string))))

(defun scoot--column-width (val)
  "Calculates the width of string value of VAL."
  (string-width (format "%s" val)))

(defun scoot-result--refresh-visual-model ()
  "Build the model backing the visual representation of the result set."
  (let* ((columns-metadata (alist-get 'columns
                                      (alist-get 'metadata
                                                 scoot-result--result-data)))
         (formatters (mapcar
                      #'scoot--resolve-formatter-from-column-metadata
                      columns-metadata))
         (headers (mapcar
                   (lambda (column-metadata)
                     (let ((name (alist-get 'name column-metadata)))
                       (list :name (alist-get 'name column-metadata)
                             :header-label
                             (funcall
                              (plist-get scoot-formatter-header
                                         :format-value)
                              name
                              column-metadata)
                             :metadata column-metadata)))
                   columns-metadata))
         (tables (cl-remove-duplicates (mapcar
                                        (lambda (h)
                                          (alist-get 'table (plist-get h :metadata)))
                                        headers)
                                       :test 'string-equal))
         (raw-table-data (alist-get 'rows scoot-result--result-data))
         (records (cl-mapcar
                   (lambda (row)
                     (cl-mapcar (lambda (cell-value fmt)
                                  (let ((formatter (if (null cell-value)
                                                       scoot-formatter-null
                                                     fmt)))
                                    (list :value cell-value
                                          :formatted-value
                                          (funcall
                                           (plist-get formatter :format-value)
                                           cell-value))))
                                row
                                formatters))
                   raw-table-data))
         (widths (cl-mapcar
                  (lambda (width header)
                    (max width (scoot--column-width (plist-get header
                                                               :header-label))))
                  (if records
                      (apply #'cl-mapcar
                             (lambda (&rest col)
                               (apply #'max
                                      (mapcar (lambda (cell)
                                                (scoot--column-width
                                                 (plist-get cell
                                                            :formatted-value)))
                                              col)))
                             records)
                    (make-list (length headers) 0))
                  headers)))
    (setq scoot-result--result-model
          (list :headers headers
                :tables tables
                :widths widths
                :formatters formatters
                :records records))))

(defun scoot-result--insert-table-divider ()
  "Insert a horizontal table divider."
  (scoot--insert-faced
   (concat "+-"
           (mapconcat (lambda (w) (make-string w ?-))
                      (plist-get scoot-result--result-model :widths)
                      "-+-")
           "-+\n")
   'scoot-table-face))

(defun scoot-result--insert-result-table-header ()
  "Insert the result set table header."
  (scoot-result--insert-table-divider)

  (cl-mapc (lambda (header width)
             (scoot--insert-faced "| " 'scoot-table-face)
             (let* ((align (plist-get scoot-formatter-header :align))
                    (name (plist-get header :name))
                    (header-label (plist-get header :header-label))
                    (padding (- width (string-width header-label))))
               (when (eq align 'right)
                 (insert (make-string padding ?\s)))
               (funcall
                (plist-get scoot-formatter-header :output-cell)
                name
                header-label)
               (when (eq align 'left)
                 (insert (make-string padding ?\s)))
               (insert " ")))
           (plist-get scoot-result--result-model :headers)
           (plist-get scoot-result--result-model :widths))
  (scoot--insert-faced "|" 'scoot-table-face)
  (insert "\n")

  (scoot-result--insert-table-divider))

(defun scoot-result--insert-table-row (row)
  "Insert the table row ROW."

  (cl-mapc (lambda (cell width fmt)
             (scoot--insert-faced "| " 'scoot-table-face)
             (let* ((value (plist-get cell :value))
                    (formatted-value (plist-get cell :formatted-value))
                    (formatter (if (null value)
                                   scoot-formatter-null
                                 fmt))
                    (align (plist-get formatter :align))
                    (padding (- width (string-width formatted-value))))
               (when (eq align 'right)
                 (insert (make-string padding ?\s)))
               (funcall
                (plist-get formatter :output-cell)
                value
                formatted-value)
               (when (eq align 'left)
                 (insert (make-string padding ?\s)))
               (insert " ")))
           row
           (plist-get scoot-result--result-model :widths)
           (plist-get scoot-result--result-model :formatters))
  (scoot--insert-faced "|" 'scoot-table-face)
  (insert "\n"))

(defun scoot-result--insert-result-set ()
  "Insert the result set table into the buffer."
  (scoot-result--refresh-visual-model)
  (scoot-result--insert-result-table-header)
  (mapc #'scoot-result--insert-table-row
        (plist-get scoot-result--result-model :records))
  (scoot-result--insert-table-divider))

(defun scoot-result--activate-outline-minor-mode ()
  "Configure and activate `outline-minor-mode`."
  (setq-local outline-level (lambda () 1)
              outline-search-function #'outline-search-level
              outline-minor-mode-cycle t
              outline-minor-mode-highlight t
              outline-minor-mode-use-buttons 'insert)
  (outline-minor-mode 1))

(defun scoot-result--deactivate-minor-mode ()
  "Wipe configuration and deactivate `outline-minor-mode`, if present."
  (dolist (var '(outline-level
                 outline-search-function
                 outline-minor-mode-cycle
                 outline-minor-mode-highlight
                 outline-minor-mode-use-buttons))
    (when (local-variable-p var)
      (kill-local-variable var)))
  (when (bound-and-true-p outline-minor-mode)
    (outline-minor-mode -1)))

(defun scoot-result--insert-buffer-info ()
  "Insert result buffer basic header information."
  (scoot--insert-faced "Connection: " 'scoot-label-face)
  (insert scoot-result--result-connection-name)
  (insert "\n"))

(defun scoot-propertize-sql (sql-string)
  "Propertize SQL-STRING with syntax highlighting via font-lock."
  (with-temp-buffer
    (erase-buffer)
    (sql-mode)
    (insert sql-string)
    (font-lock-ensure)
    (buffer-string)))

(defun scoot-insert-propertized-string (s)
  "Insert propertized string S into current buffer, preserving its text properties.

This feels like a nasty hack, but ensures that the inserted text retains its
font-lock properties."
  (let ((start (point)))
    (insert s)
    (let ((i 0)
          (len (length s)))
      (while (< i len)
        (let ((props (text-properties-at i s)))
          (when props
            (add-text-properties (+ start i) (+ start i 1) props)))
        (setq i (1+ i))))))

(defun scoot-result-refresh-buffer ()
  "Redraw the entire buffer from scoot-result--result-model."
  (interactive)
  (scoot-result--deactivate-minor-mode)
  (read-only-mode -1)
  (erase-buffer)

  (setq-local scoot-result--outline-sections 0)

  (scoot-result--insert-buffer-info)
  (insert "\n")

  (when (eq scoot-result--result-type 'object)
    (scoot--insert-faced (concat (pcase scoot-result--result-object-type
                                   ('table "Table")
                                   ('schema "Schema")
                                   ('database "Database"))
                                 ": ")
                         'scoot-label-face)
    (insert scoot-result--result-object-name)
    (insert "\n\n"))

  (when (eq scoot-result--result-type 'query)
    (scoot--insert-faced "Query: " 'scoot-label-face)
    (insert "\n")
    (scoot--insert-query-box (format "%s" scoot-result--current-sql-statement))
    (insert "\n"))

  (scoot-result--insert-result-set)

  (insert "\n\n")

  (when-let ((sql-meta (alist-get 'sql (alist-get 'metadata scoot-result--result-data))))
    (dolist (sql-entry sql-meta)
      (insert (propertize (car sql-entry)
                          'face 'scoot-outline-header-face
                          'outline-level 1))
      (insert "\n")
      (scoot-insert-propertized-string
       (scoot-propertize-sql
        (cdr sql-entry)))
      (insert "\n")
      (setq-local scoot-result--outline-sections (1+ scoot-result--outline-sections))))

  (unless (zerop scoot-result--outline-sections)
    (scoot-result--activate-outline-minor-mode))

  (read-only-mode 1))

(cl-defun scoot-result--tables-in-result (&allow-other-keys)
  "Describe a table, either TABLE-NAME or tables involved in the query/result."
  (interactive)
  (let* ((result-tables (plist-get scoot-result--result-model :tables))
         (table-count (length result-tables)))
    (cond
     ((eq table-count 1)
      (car result-tables))

     ((> table-count 1)
      (scoot-connection--table-prompt :tables result-tables)))))

(defun scoot-result--open-result-buffer (result-context)
  "Open a Scoot Result Buffer with a result described by RESULT-CONTEXT.

RESULT-CONTEXT is expected to be a plist, with the following possible keys:
:result - contains the headers, rows and metadata of the result.
:connection - describes the connection used to retrieve this result.
:type - the type of result (query/objects/object).

Additional keys for type query:
:statement - The SQL statement that produced the result.

Additional keys for type object/objects:
:object-type - The type of object (table/schema/database).

Additional keys for type object:
:object-name - The name of the object described."

  (let ((result (plist-get result-context :result))
        (connection (plist-get result-context :connection))
        (stmt (plist-get result-context :statement))
        (type (plist-get result-context :type))
        (object-type (plist-get result-context :object-type))
        (object-name (plist-get result-context :object-name)))

    ;; FIXME: Decide on a scheme to uniquely identify result buffers
    (let ((buf (get-buffer-create "*scoot-result*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (scoot-result-mode)
        (setq-local scoot-result--result-connection-name connection
                    scoot-result--result-type type
                    scoot-result--result-object-type object-type
                    scoot-result--result-object-name object-name
                    scoot-result--original-sql-statement stmt
                    scoot-result--current-sql-statement stmt
                    scoot-result--result-data result)

        (scoot-result-refresh-buffer)
        (unless (zerop scoot-result--outline-sections)
          (outline-hide-subtree))
        (display-buffer buf)))))

(defvar scoot-result-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'scoot-result-refresh-buffer)
    (define-key map (kbd "TAB") #'outline-toggle-children)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    map)
  "Keymap for `scoot-result-mode`.")

(define-derived-mode scoot-result-mode special-mode "Scoot Result"
  "Major mode for displaying and interacting with SQL resultsets."
  (setq-local scoot-local--connection-name-resolvers '((lambda () scoot-result--result-connection-name)))
  (setq-local scoot-local--table-name-resolvers '(scoot-result--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local truncate-lines t)
  (setq buffer-read-only t)
  (scoot-ensure-server))

(provide 'scoot-result)

;;; scoot-result.el ends here
