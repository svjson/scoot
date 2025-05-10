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
(require 'scoot-query-block)
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

  (cl-mapc (lambda (header cell width fmt)
             (scoot--insert-faced "| " 'scoot-table-face)
             (let* ((cell-start (- (point) 1))
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
        (display-buffer buf)))))

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
  (add-hook 'post-command-hook 'scoot-result--check-cursor-position nil t))

(provide 'scoot-result)

;;; scoot-result.el ends here
