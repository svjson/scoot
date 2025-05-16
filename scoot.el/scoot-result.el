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
(require 'xref)
(require 'scoot-query-block)
(require 'scoot-server)
(require 'scoot-connection)
(require 'scoot-xref)

(defcustom scoot-primary-key-icon "🔑"
  "Icon used to indicate that a column has a primary key constraint."
  :type 'string
  :group 'scoot)

(defcustom scoot-foreign-key-icon "🗝️"
  "Icon used to indicate that a column has a foreign key constraint."
  :type 'string
  :group 'scoot)

(defconst scoot-result--buffer-default-name "*scoot result*"
  "The default result buffer name to use if all other naming methods fail.")

(defvar scoot-result-buffer-default-name scoot-result--buffer-default-name)

(defvar scoot-generate-result-buffer-name-function 'scoot-result--generate-buffer-name
  "Function used to generate scoot result buffer names.")

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

(defvar-local scoot-result--outline-sections nil
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

(declare-function which-key-add-keymap-based-replacements nil)
(declare-function which-key-add-key-based-replacements nil)

(declare-function scoot-describe-table "scoot")
(declare-function scoot-list-databases "scoot")
(declare-function scoot-list-schemas "scoot")
(declare-function scoot-list-tables "scoot")

(defun scoot--insert-faced (text face)
  "Insert TEXT with FACE applied as a text property."
  (let ((beg (point)))
    (insert text)
    (put-text-property beg (point) 'face face)))

(defun scoot--value-to-string (val &rest _)
  "Stringify a value, VAL, for display in a result set table."
  (if (null val)
      "NULL"
    (format "%s" val)))

(defvar scoot-formatter-header
  (list :align 'left
        :format-value (lambda (name metadata)
                        (format "%s%s"
                                (cond ((eq t (alist-get 'primary_key metadata))
                                       scoot-primary-key-icon)

                                      ((cl-some (lambda (con) (equal (alist-get 'type con) "fk")) (alist-get 'constraints metadata))
                                       scoot-foreign-key-icon)

                                      (t ""))
                                name))
        :output-cell (lambda (_ formatted-value)
                       (scoot--insert-faced formatted-value 'scoot-header-face))))

(defvar scoot-formatter-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-string-face))
        :sql-literal
        (lambda (value)
          (concat "'" value "'"))))

(defvar scoot-formatter-number
  (list :align 'right
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-number-face))
        :sql-literal
        #'identity))

(defvar scoot-formatter-boolean
  (list :align 'right
        :format-value (lambda (val &rest _)
                        (if (eq val t) "true" "false"))
        :output-cell
        (lambda (val formatted-value)
          (scoot--insert-faced formatted-value
                               (if (eq val t)
                                   'scoot-cell-boolean-true-face
                                 'scoot-cell-boolean-false-face)))
        (lambda (value)
          (cond
           (value "TRUE")
           (t "FALSE")))))

(defvar scoot-formatter-raw-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-generic-face))
        :sql-literal
        (lambda (value)
          (concat "'" value "'"))))

(defvar scoot-formatter-null
  (list :align 'right
        :format-value (lambda (_) "NULL")
        :output-cell
        (lambda (_ formatted-value)
          (scoot--insert-faced formatted-value 'scoot-cell-null-face))
        :sql-literal
        (lambda (_) "NULL")))

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
  "Calculates the width of string value of VAL.

Uses `string-pixel-width' of the string representation of VAL to account
for special characters and icons/emojis that do not align with the the
default width of the, presumably, otherwise fixed-width font."
  (if (display-graphic-p)
      (let* ((pixel-width (string-pixel-width (format "%s" val)))
             (char-width (default-font-width)))
        (/ pixel-width char-width))
    (string-width (format "%s" val))))

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
             (let* ((header-begin (1- (point)))
                    (align (plist-get scoot-formatter-header :align))
                    (name (plist-get header :name))
                    (header-label (plist-get header :header-label))
                    (padding (- width (scoot--column-width header-label))))
               (when (eq align 'right)
                 (insert (make-string padding ?\s)))
               (funcall
                (plist-get scoot-formatter-header :output-cell)
                name
                header-label)
               (when (eq align 'left)
                 (insert (make-string padding ?\s)))
               (insert " ")
               (add-text-properties header-begin (point) (list 'thing 'table-header
                                                               'header name
                                                               'column (alist-get 'column (plist-get header :metadata))
                                                               'table (alist-get 'table (plist-get header :metadata))))))
           (plist-get scoot-result--result-model :headers)
           (plist-get scoot-result--result-model :widths))
  (scoot--insert-faced "|" 'scoot-table-face)
  (insert "\n")

  (scoot-result--insert-table-divider))

(defun scoot-result--insert-table-row (row)
  "Insert the table row ROW."

  (cl-mapc (lambda (header cell width fmt)
             (scoot--insert-faced "| " 'scoot-table-face)
             (let* ((cell-start (1- (point)))
                    (value (plist-get cell :value))
                    (formatted-value (plist-get cell :formatted-value))
                    (formatter (if (null value)
                                   scoot-formatter-null
                                 fmt))
                    (align (plist-get formatter :align))
                    (padding (- width (string-width formatted-value)))
                    (header-meta (plist-get header :metadata))
                    (cell-props (list 'thing 'table-cell
                                      'column-meta header-meta
                                      'formatter formatter
                                      'value value)))
               (when (eq align 'right)
                 (insert (make-string padding ?\s)))
               (funcall
                (plist-get formatter :output-cell)
                value
                formatted-value)
               (when (eq align 'left)
                 (insert (make-string padding ?\s)))
               (insert " ")
               (add-text-properties cell-start (point) cell-props)))
           (plist-get scoot-result--result-model :headers)
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

(defun scoot-result--insert-property-grid (info)
  "Insert a grid of properties displayed as \"Property: Value\".

INFO contains the grid information as a plist with the following properties:
:columns - The number of columns to display
:data - A list of plists containing the properties."
  (let* ((columns (plist-get info :columns))
         (column-min-width 0)
         (entries (mapcar (lambda (entry)
                            (let* ((value (format "%s" (plist-get entry :value)))
                                   (width (length (concat (plist-get entry :label)
                                                          ": "
                                                          value)))
                                   (updated (scoot--plist-merge
                                             entry
                                             (list :value
                                                   value
                                                   :min-width
                                                   width))))
                              (when (> width column-min-width)
                                (setq column-min-width width))
                              updated))
                          (plist-get info :data)))
         (column-width (/ (window-width) (plist-get info :columns))))
    (let ((index 1))
      (mapcar
       (lambda (entry)
         (let ((meta (plist-get entry :meta)))
           (scoot--insert-faced (propertize (plist-get entry :label)
                                            'role 'data-label
                                            'meta meta)
                                'scoot-label-face)
           (insert (propertize ": "
                               'role 'data-label
                               'meta meta))
           (insert (propertize (plist-get entry :value)
                               'role 'data-value
                               'meta meta))
           (if (zerop (% index columns))
               (insert "\n")
             (insert (make-string (- column-width (plist-get entry :min-width)) ?\s)))
           (setq index (1+ index))))
       entries))))

(defun scoot-result--insert-buffer-info ()
  "Insert result buffer basic header information."
  (let ((connection (gethash scoot-result--result-connection-name scoot-connections)))

    (scoot-result--insert-property-grid
     (list :columns 2
           :data (list (list :label "Connection"
                             :value (plist-get connection :name)
                             :meta 'name)
                       (list :label "Database"
                             :value (plist-get connection :database)
                             :meta 'database)
                       (list :label "Host"
                             :value (plist-get connection :host)
                             :meta 'host)
                       (list :label "Port"
                             :value (plist-get connection :port)
                             :meta 'port)
                       (list :label "Dialect"
                             :value (plist-get connection :dialect)
                             :meta 'dialect)
                       (list :label "Driver"
                             :value (plist-get connection :driver)
                             :meta 'driver))))))

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
  (scoot--save-cursor)
  (scoot-result--deactivate-minor-mode)
  (read-only-mode -1)
  (erase-buffer)

  (setq-local scoot-result--outline-sections 0)

  (scoot-result--insert-buffer-info)
  (insert "\n")

  (when (eq scoot-result--result-type 'object)
    (insert (propertize (concat (pcase scoot-result--result-object-type
                                   ('table "Table")
                                   ('schema "Schema")
                                   ('database "Database"))
                                ": ")
                        'face 'scoot-label-face))
    (insert (propertize scoot-result--result-object-name
                        'thing (intern (concat (symbol-name scoot-result--result-object-type)
                                               "-name"))))
    (insert "\n\n"))

  (when (eq scoot-result--result-type 'query)
    (scoot--insert-faced "Query: " 'scoot-label-face)
    (insert "\n")
    (scoot-qb--insert-query-block (format "%s" scoot-result--current-sql-statement))
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

  (read-only-mode 1)
  (scoot--restore-cursor))

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

(defun scoot-result--generate-buffer-name (connection
                                           _result
                                           _stmt
                                           type
                                           object-type
                                           object-name)
  "Generate a buffer named based on the result to show.

CONNECTION is the connection used to retrieve the result.
RESULT is the result to show in the buffer.
STMT is the SQL statement that produced the result.
TYPE is the type of result (query/objects/object).
OBJECT-TYPE is the type of object (table/schema/database).
OBJECT-NAME is the name of the object described."
  (concat (format "*scoot(%s)" connection)
          (pcase type
            ('query ": query")
            ('object (concat " "
                             (scoot--object-type-name object-type)
                             ": "
                             object-name))
            ('objects (concat ": " (scoot--object-type-name object-type t))))
          "*"))

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
    (let* ((buf-name (cond ((null scoot-generate-result-buffer-name-function) scoot-result-buffer-default-name)
                           ((symbolp scoot-generate-result-buffer-name-function)
                            (let ((gen-fn (symbol-function scoot-generate-result-buffer-name-function)))
                              (if (functionp gen-fn)
                                  (funcall gen-fn connection result stmt type object-type object-name)
                                scoot-result-buffer-default-name)))
                           (t scoot-result-buffer-default-name)))
           (buf (get-buffer-create (or buf-name scoot-result--buffer-default-name))))
      (with-current-buffer buf
        (read-only-mode -1)
        (scoot-result-mode)
        (setq-local scoot-result--result-connection-name connection
                    scoot-result--result-type type
                    scoot-result--result-object-type object-type
                    scoot-result--result-object-name object-name
                    scoot-result--result-data result)

        (unless scoot-result--original-sql-statement
          (setq-local scoot-result--original-sql-statement stmt))

        (setq-local scoot-result--current-sql-statement
                    (or (alist-get 'stmt (alist-get 'metadata result))
                        stmt))

        (scoot-result-refresh-buffer)
        (unless (zerop scoot-result--outline-sections)
          (save-excursion
            (goto-char (point-max))
            (outline-hide-subtree)))
        (display-buffer buf))
      buf)))

(defun scoot-result--check-cursor-position ()
  "Check cursor position and handle query block activation/deactivation."
  (if (scoot-qb--query-block-at-point-p)
      (unless scoot-query-block-mode (scoot-query-block-mode 1))
    (when scoot-query-block-mode (scoot-query-block-mode -1))))

(defun scoot-result--cell-at-point ()
  "Return the result set table cell at point."
  (let* ((props (scoot--props-at-point)))
    (list :type (alist-get 'thing props)
          :column (alist-get 'column-meta props)
          :value (alist-get 'value props)
          :formatter (alist-get 'formatter props))))

(defun scoot-result--buffer-connection ()
  "Get the connection used to retrieve the current result."
  (gethash scoot-result--result-connection-name scoot-connections))

(defun scoot-result--execute-query ()
  "Execute the current query in this result buffer."
  (interactive)
  (setq scoot-result--current-sql-statement (scoot-qb--get-query))
  (scoot-connection--execute-statement
   (scoot-result--buffer-connection)
   scoot-result--current-sql-statement
   #'scoot-result--open-result-buffer))

(defun scoot-result--modify-where (op cmp &optional allow-null)
  "Perform a modification of the WHERE-clause using the table cell at point.

OP is either `add or `remove.
CMP is the comparison to do in the WHERE-clause, ie \"=\" or \">\".
ALLOW-NULL signals whether this operation is legal for NULL values."
  (interactive)
  (let ((cell (scoot-result--cell-at-point)))
    (when-let ((type (plist-get cell :type))
               (column (plist-get cell :column))
               (formatter (plist-get cell :formatter)))
      (let* ((value (plist-get cell :value)))
        (when (or value allow-null)
          (scoot-connection--modify-statement
           (scoot-result--buffer-connection)
           scoot-result--current-sql-statement
           `("WHERE" ,op)
           (list (alist-get 'name column)
                 (if value cmp
                   (pcase cmp
                     ("=" "IS")
                     ("!=" "IS NOT")))
                 (funcall
                  (plist-get formatter :sql-literal)
                  value))
           #'scoot-result--open-result-buffer))))))

(cl-defmethod scoot-xref--point-is-thing-p ((_type (eql 'column)) _point xref-target props)
  "Test PROPS for thing of TYPE `column against XREF-TARGET."
  (when (equal scoot-result--result-object-type 'table)
    (let* ((column-meta (alist-get 'column-meta props) ))
      (and (equal (alist-get 'thing props) 'table-cell)
           (equal (alist-get 'type column-meta) "OBJECT-NAME")
           (equal (alist-get 'value props) (plist-get xref-target :column))))))

(cl-defmethod scoot-xref--point-is-thing-p ((_type (eql 'table)) _point xref-target props)
  "Test PROPS for ting of TYPE `table against XREF-TARGET."
  (and (equal scoot-result--result-object-type 'table)
       (equal scoot-result--result-object-name (plist-get xref-target :table))
       (equal (alist-get 'thing props) 'table-name)))

(defun scoot-result--xref-action (xref-target)
  "Find location for XREF-TARGET."
  (let* ((xref-conn-cache (scoot-xref--get-conn-cache (scoot-result--buffer-connection)))
         (identifier (plist-get xref-target :xref))
         (cached-xref (gethash identifier xref-conn-cache)))
    (if (scoot-xref--is-valid-xref cached-xref xref-target)
        (list cached-xref)
      (let* ((target (plist-get xref-target :target))
             (t-table (plist-get xref-target :table))
             (xref-async-response
              (xref-make identifier
                         (make-scoot-xref-async-location :identifier identifier :marker nil :pending t))))
        (cond
         ((or (member target '(:column :table)))
          (progn (scoot-connection--describe-table (scoot-result--buffer-connection)
                                                   t-table
                                                   (lambda (result-context)
                                                     (let* ((buf (scoot-result--open-result-buffer result-context))
                                                            (pos (scoot-xref--find-buffer-location buf xref-target))
                                                            (loc (xref-make-buffer-location buf pos)))
                                                       (puthash identifier
                                                                (xref-make identifier loc)
                                                                xref-conn-cache)
                                                       (xref-find-definitions identifier))))
                 (list xref-async-response))))))))

(defun scoot-result--xref-id-at-point ()
  "Resolve identifier for xref at point."
  (let ((props (scoot--props-at-point)))
    (cond
     ((eq 'query scoot-result--result-type)
      (pcase (alist-get 'thing props)
        ('table-header (scoot-xref--column-identifier (list :column (alist-get 'column props)
                                                            :name (alist-get 'name props)
                                                            :table (alist-get 'table props))))))
     ((eq 'tables scoot-result--result-object-type)
      (pcase (alist-get 'thing props)
        ('table-cell (scoot-xref--table-identifier
                      (list :name (alist-get 'value props))))))

     ((eq 'databases scoot-result--result-object-type)
      (pcase (alist-get 'thing props)
        ('table-cell (scoot-xref--database-identifier
                      (list :name (alist-get 'value props)))))))))

(defun scoot-result--xref-completions ()
  "Identifiers to show in xref completion prompt."
  (cond
   ((eq 'query scoot-result--result-type)
    (append (mapcar (lambda (tbl) (scoot-xref--table-identifier (list :name tbl)))
                    (plist-get scoot-result--result-model :tables))
            (mapcar
             (lambda (h)
               (let ((md (plist-get h :metadata)))
                 (scoot-xref--column-identifier (list :column (alist-get 'column md)
                                                      :name (alist-get 'name md)
                                                      :table (alist-get 'table md)))))
             (plist-get scoot-result--result-model :headers))))
   ((eq 'tables scoot-result--result-object-type)
    (append (mapcar (lambda (row) (scoot-xref--table-identifier (list :name (plist-get (car row) :value))))
                    (plist-get scoot-result--result-model :records))))))

(defun scoot-result--add-or-remove-prefix-map (op)
  "Generate a prefixed keymap for adding or removing to/from the query.

OP is either `add or `remove."
  (let ((map (make-sparse-keymap))
        (where-prefix (make-sparse-keymap))
        (op-name (if (eq op 'add) "Add" "Remove"))
        (op-dir (if (eq op 'add) "to" "from")))
    (define-key where-prefix (kbd "e")
                (lambda () (interactive) (scoot-result--modify-where op "=" t)))
    (define-key where-prefix (kbd "=")
                (lambda () (interactive) (scoot-result--modify-where op "=" t)))
    (define-key where-prefix (kbd "!")
                (lambda () (interactive) (scoot-result--modify-where op "!=" t)))
    (define-key where-prefix (kbd "n")
                (lambda () (interactive) (scoot-result--modify-where op "!=" t)))
    (define-key where-prefix (kbd "<")
                (lambda () (interactive) (scoot-result--modify-where op "<")))
    (define-key where-prefix (kbd ">")
                (lambda () (interactive) (scoot-result--modify-where op ">")))
    (define-key where-prefix (kbd "l")
                (lambda () (interactive) (scoot-result--modify-where op "<=")))
    (define-key where-prefix (kbd "g")
                (lambda () (interactive) (scoot-result--modify-where op ">=")))
    (define-key map (kbd "w") where-prefix)

    (when (featurep 'which-key)
      (which-key-add-keymap-based-replacements map
        "w" (concat op-name " " op-dir " WHERE clause"))

      (which-key-add-keymap-based-replacements where-prefix
        "e" (concat op-name " = " op-dir " WHERE-clause")
        "<" (concat op-name " < " op-dir " WHERE-clause")
        ">" (concat op-name " > " op-dir " WHERE-clause")))
    map))

(defvar scoot-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'scoot-result-refresh-buffer)
    (define-key map (kbd "h") #'describe-mode)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "TAB") #'outline-toggle-children)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    (define-key map (kbd "C-c C-c") #'scoot-result--execute-query)
    (define-key map (kbd "a") (scoot-result--add-or-remove-prefix-map 'add))
    (define-key map (kbd "r") (scoot-result--add-or-remove-prefix-map 'remove))
    map)
  "Keymap for `scoot-result-mode`.")

(define-derived-mode scoot-result-mode fundamental-mode "Scoot Result"
  "Major mode for displaying and interacting with SQL resultsets."
  (setq-local scoot-local--connection-name-resolvers '((lambda () scoot-result--result-connection-name)))
  (setq-local scoot-local--table-name-resolvers '(scoot-result--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local truncate-lines t)
  (setq buffer-read-only t)
  (add-hook 'post-command-hook 'scoot-result--check-cursor-position nil t)

  (setq-local scoot-xref-action-fn 'scoot-result--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-result--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-result--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t))

(provide 'scoot-result)

;;; scoot-result.el ends here
