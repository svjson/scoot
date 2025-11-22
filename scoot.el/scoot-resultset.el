;;; scoot-resultset.el --- summary -*- lexical-binding: t -*-

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
;; `scoot-resultset-mode`, a read-only major mode for displaying SQL query
;; results in a structured, grid-like format.
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

(require 'scoot-buffer)
(require 'scoot-xref)



;; Forward declarations

(declare-function which-key-add-keymap-based-replacements nil)
(declare-function which-key-add-key-based-replacements nil)

(declare-function scoot-describe-table "scoot")
(declare-function scoot-list-databases "scoot")
(declare-function scoot-list-schemas "scoot")
(declare-function scoot-list-tables "scoot")



;; Customization Options

(defcustom scoot-resultset-buffer-default-name "*scoot result*"
  "The default result buffer name to use if all other naming methods fail."
  :type 'string
  :group 'scoot)

(defcustom scoot-generate-resultset-buffer-name-function 'scoot-rs--generate-buffer-name
  "Function used to generate scoot result buffer names."
  :type 'function
  :group 'scoot)



;; Local Variables

(defvar-local scoot-rs--original-sql-statement nil
  "The original SQL statement that the current buffer is based on.")

(defvar-local scoot-rs--current-sql-statement nil
  "The current SQL statement that the current buffer is based on.")

(defvar-local scoot-rs--result-type nil
  "The type of result displayed in the current buffer.

Valid values are:
query
objects
object")

(defvar-local scoot-rs--result-object-type nil
  "The type of object listed in the current buffer.
Only used/valid when `scoot-rs--result-type` is `objects`.

Valid values are:
database
schema
table")

(defvar-local scoot-rs--result-object-name nil
  "The name of the object(table/schema/database) described in the buffer.")



;; Query

(defun scoot-rs--execute-query (&optional query)
  "Execute QUERY or the current query in this result buffer."
  (interactive)
  (setq scoot-rs--current-sql-statement (or query (scoot-qb--get-query)))
  (scoot-connection--execute-statement
   scoot-buffer-connection
   scoot-rs--current-sql-statement
   #'scoot-rs--update))


(cl-defun scoot-rs--tables-in-result (&allow-other-keys)
  "Describe a table, either TABLE-NAME or tables involved in the query/result."
  (interactive)
  (let* ((result-tables (plist-get scoot-table--table-model :tables))
         (table-count (length result-tables)))
    (cond
     ((eq table-count 1)
      (car result-tables))

     ((> table-count 1)
      (scoot-connection--table-prompt :tables result-tables)))))



;; Query modification

(defun scoot-rs--modify-select (op)
  "Perform a modification of the SELECT-clause using the table cell at point.

OP is either `add or `remove."
  (interactive)
  (let ((cell (scoot-table--cell-at-point)))
    (when-let ((col (plist-get cell :column))
               (column (alist-get 'column col))
               (name (alist-get 'name col)))
      (scoot-connection--modify-statement
       scoot-buffer-connection
       scoot-rs--current-sql-statement
       `("SELECT" ,op)
       (list (list (cons 'name name)
                   (cons 'column column)))
       #'scoot-rs--update)))
  nil)

(defun scoot-rs--modify-where (op cmp &optional allow-null)
  "Perform a modification of the WHERE-clause using the table cell at point.

OP is either `add or `remove.
CMP is the comparison to do in the WHERE-clause, ie \"=\" or \">\".
ALLOW-NULL signals whether this operation is legal for NULL values."
  (interactive)
  (let ((cell (scoot-table--cell-at-point)))
    (when-let ((type (plist-get cell :type))
               (column (plist-get cell :column))
               (formatter (plist-get cell :formatter)))
      (let* ((value (plist-get cell :value)))
        (when (or value allow-null)
          (scoot-connection--modify-statement
           scoot-buffer-connection
           scoot-rs--current-sql-statement
           `("WHERE" ,op)
           (list (alist-get 'name column)
                 (if value cmp
                   (pcase cmp
                     ("=" "IS")
                     ("!=" "IS NOT")))
                 (funcall
                  (plist-get formatter :sql-literal)
                  value))
           #'scoot-rs--update))))))



;; ResultSet buffer

(defun scoot-rs--generate-buffer-name (connection
                                       _result
                                       _stmt
                                       type
                                       object-type
                                       object-name
                                       override)
  "Generate a buffer named based on the result to show.

CONNECTION is the connection used to retrieve the result.
RESULT is the result to show in the buffer.
STMT is the SQL statement that produced the result.
TYPE is the type of result (query/objects/object).
OBJECT-TYPE is the type of object (table/schema/database).
OBJECT-NAME is the name of the object described.
OVERRIDE can be used to bypass other generation rules"
  (concat (format "*scoot(%s)" (plist-get connection :name))
          (if override
              (format ": %s" override)
            (pcase type
              ('query ": query")
              ('object (concat " "
                               (scoot--object-type-name object-type)
                               ": "
                               object-name))
              ('objects (concat ": " (scoot--object-type-name object-type t)))))
          "*"))



;; ResultSet

(defun scoot-rs--update (result-context)
  "Update the current resultset buffer with a query execution result.

RESULT-CONTEXT contains the query statement, result data and connection."
  (let* ((result (plist-get result-context :result))
         (stmt (or (alist-get 'stmt (alist-get 'metadata result))
                   (plist-get result-context :statement)))
         (object-type (plist-get result-context :object-type))
         (object-name (plist-get result-context :object-name))
         (type (plist-get result-context :type)))
    (setq-local scoot-buffer-connection (plist-get result-context :connection)
                scoot-rs--result-type type
                scoot-rs--current-sql-statement stmt
                scoot-rs--result-object-type object-type
                scoot-rs--result-object-name object-name
                scoot-rs--result-data result
                scoot-buffer-sections
                (list 'connection-header
                      (list :type 'query-editor
                            :current-sql stmt)
                      (list :type 'data-table
                            :data result
                            :editablep t)))

    (unless scoot-rs--original-sql-statement
      (setq-local scoot-rs--original-sql-statement stmt))

    (scoot-buffer-refresh)))



;; Scoot ResultSet Entry Points

(defun scoot--open-resultset (result-context)
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
:object-name - The name of the object described."
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
        (scoot-resultset-mode)
        (scoot-rs--update result-context)
        (display-buffer buf))
      buf)))


(defun scoot-rs--open-object-list-resultset (object-type title)
  "Interactively list objects visible to user of the current connection.

Valid object types are:
\\='databases
\\='schemas
\\='tables

This will open the resulting list of objects of OBJECT-TYPE in a result
buffer with the provided TITLE.

OBJECT-TYPE is the type of the object to list.
CONNECTION (Optional)"
  (let ((connection (scoot--interactive-resolve-connection)))
    (scoot--list-objects object-type
                         title
                         connection
                         (lambda (object-result)
                           (scoot--open-resultset
                            (list :type 'objects
                                  :object-type object-type
                                  :result
                                  `((columns . [,title])
                                    (rows . ,(mapcar #'vector
                                                     (seq-into
                                                      (alist-get object-type object-result)
                                                      'vector)))
                                    (metadata . ((columns . [((name . ,title)
                                                              (type . "OBJECT-NAME"))]))))
                                  :connection connection))))))



;; xref implementation for resultset buffers

(cl-defmethod scoot-xref--point-is-thing-p ((_type (eql 'column)) _point xref-target props)
  "Test PROPS for thing of TYPE `column against XREF-TARGET."
  (when (equal scoot-rs--result-object-type 'table)
    (let* ((column-meta (alist-get 'column-meta props) ))
      (and (equal (alist-get 'thing props) 'table-cell)
           (equal (alist-get 'type column-meta) "OBJECT-NAME")
           (equal (alist-get 'value props) (plist-get xref-target :column))))))

(cl-defmethod scoot-xref--point-is-thing-p ((_type (eql 'table)) _point xref-target props)
  "Test PROPS for thing of TYPE `table against XREF-TARGET."
  (and (equal scoot-rs--result-object-type 'table)
       (equal scoot-rs--result-object-name (plist-get xref-target :table))
       (equal (alist-get 'thing props) 'table-name)))

(cl-defmethod scoot-xref--point-is-thing-p ((_type (eql 'expr)) _point xref-target props)
  "Test PROPS for thing of TYPE `expr against XREF-TARGET."
  (and (equal (alist-get 'thing props) 'table-cell)
       (cl-some (lambda (expr)
                  (equal
                   (plist-get expr :lhs)
                   (alist-get 'column (alist-get 'column-meta props))))
                (plist-get xref-target :expr))))

(defun scoot-rs--xref-action (xref-target)
  "Find location for XREF-TARGET."
  (let* ((xref-conn-cache (scoot-xref--get-conn-cache scoot-buffer-connection))
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
          (progn (scoot-connection--describe-table scoot-buffer-connection
                                                   t-table
                                                   (lambda (result-context)
                                                     (let* ((buf (scoot--open-resultset result-context))
                                                            (pos (scoot-xref--find-buffer-location buf xref-target))
                                                            (loc (xref-make-buffer-location buf pos)))
                                                       (puthash identifier
                                                                (xref-make identifier loc)
                                                                xref-conn-cache)
                                                       (xref-find-definitions identifier))))
                 (list xref-async-response)))
         ((equal target :expr)
          (progn (scoot-connection--execute-statement scoot-buffer-connection
                                                      (concat
                                                       (format "SELECT * FROM %s WHERE " (plist-get xref-target :table))
                                                       (string-join
                                                        (mapcar
                                                         (lambda (expr)
                                                           (concat (plist-get expr :lhs)
                                                                   (plist-get expr :oper)
                                                                   (plist-get expr :rhs)))
                                                         (plist-get xref-target :expr))
                                                        " AND "))
                                                      (lambda (result-context)
                                                        (let* ((buf (scoot--open-resultset result-context))
                                                               (pos (scoot-xref--find-buffer-location buf xref-target))
                                                               (loc (xref-make-buffer-location buf pos)))
                                                          (puthash identifier
                                                                   (xref-make identifier loc)
                                                                   xref-conn-cache)
                                                          (xref-find-definitions identifier))))
                 (list xref-async-response))))))))

(defun scoot-rs--xref-resolve-table-cell-identifier (props)
  "Analyze the cell PROPS and determine the identifier type, if any."
  (let* ((col-meta (alist-get 'column-meta props))
         (ref (alist-get 'reference (seq-find
                                     (lambda (con)
                                       (equal (alist-get 'type con) "fk"))
                                     (alist-get 'constraints col-meta)))))
    (cond
     (ref (scoot-xref--record-reference-identifier
           (list :table (alist-get 'table ref)
                 :columns (alist-get 'columns ref)
                 :value (alist-get 'value props)))))))

(defun scoot-rs--xref-id-at-point ()
  "Resolve identifier for xref at point."
  (let ((props (scoot--props-at-point)))
    (cond
     ((eq 'query scoot-rs--result-type)
      (pcase (alist-get 'thing props)
        ('table-header (scoot-xref--column-identifier (list :column (alist-get 'column props)
                                                            :name (alist-get 'name props)
                                                            :table (alist-get 'table props))))
        ('table-cell (scoot-rs--xref-resolve-table-cell-identifier props))))
     ((eq 'tables scoot-rs--result-object-type)
      (pcase (alist-get 'thing props)
        ('table-cell (scoot-xref--table-identifier
                      (list :name (alist-get 'value props))))))

     ((eq 'databases scoot-rs--result-object-type)
      (pcase (alist-get 'thing props)
        ('table-cell (scoot-xref--database-identifier
                      (list :name (alist-get 'value props)))))))))

(defun scoot-rs--xref-completions ()
  "Identifiers to show in xref completion prompt."
  (cond
   ((eq 'query scoot-rs--result-type)
    (append (mapcar (lambda (tbl) (scoot-xref--table-identifier (list :name tbl)))
                    (plist-get scoot-table--table-model :tables))
            (mapcar
             (lambda (h)
               (let ((md (plist-get h :metadata)))
                 (scoot-xref--column-identifier (list :column (alist-get 'column md)
                                                      :name (alist-get 'name md)
                                                      :table (alist-get 'table md)))))
             (plist-get scoot-table--table-model :headers))))
   ((eq 'tables scoot-rs--result-object-type)
    (append (mapcar (lambda (row) (scoot-xref--table-identifier (list :name (plist-get (car row) :value))))
                    (plist-get scoot-table--table-model :records))))))



;; Scoot ResultSet Mode - scoot-resultset-mode

(defun scoot-rs--add-or-remove-prefix-map (op)
  "Generate a prefixed keymap for adding or removing to/from the query.

OP is either `add or `remove."
  (let ((map (make-sparse-keymap))
        (where-prefix (make-sparse-keymap))
        (op-name (if (eq op 'add) "Add" "Remove"))
        (op-dir (if (eq op 'add) "to" "from")))
    (define-key where-prefix (kbd "e")
                (lambda () (interactive) (scoot-rs--modify-where op "=" t)))
    (define-key where-prefix (kbd "=")
                (lambda () (interactive) (scoot-rs--modify-where op "=" t)))
    (define-key where-prefix (kbd "!")
                (lambda () (interactive) (scoot-rs--modify-where op "!=" t)))
    (define-key where-prefix (kbd "n")
                (lambda () (interactive) (scoot-rs--modify-where op "!=" t)))
    (define-key where-prefix (kbd "<")
                (lambda () (interactive) (scoot-rs--modify-where op "<")))
    (define-key where-prefix (kbd ">")
                (lambda () (interactive) (scoot-rs--modify-where op ">")))
    (define-key where-prefix (kbd "l")
                (lambda () (interactive) (scoot-rs--modify-where op "<=")))
    (define-key where-prefix (kbd "g")
                (lambda () (interactive) (scoot-rs--modify-where op ">=")))
    (define-key map (kbd "w") where-prefix)
    (when (eq op 'remove)
      (define-key map (kbd "s") (lambda () (interactive) (scoot-rs--modify-select op))))

    (when (featurep 'which-key)
      (when (eq op 'remove)
        (which-key-add-keymap-based-replacements map
          "s" (concat op-name " " op-dir " SELECT clause")))

      (which-key-add-keymap-based-replacements map
        "w" (concat op-name " " op-dir " WHERE clause"))

      (which-key-add-keymap-based-replacements where-prefix
        "e" (concat op-name " = " op-dir " WHERE-clause")
        "<" (concat op-name " < " op-dir " WHERE-clause")
        ">" (concat op-name " > " op-dir " WHERE-clause")))
    map))

(defvar scoot-resultset-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map scoot-buffer-mode-map)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    (define-key map (kbd "C-c C-c") #'scoot-rs--execute-query)
    (define-key map (kbd "a") (scoot-rs--add-or-remove-prefix-map 'add))
    (define-key map (kbd "r") (scoot-rs--add-or-remove-prefix-map 'remove))
    map)
  "Keymap for `scoot-result-mode`.")

(define-derived-mode scoot-resultset-mode scoot-buffer-mode "Scoot ResultSet"
  "Major mode for displaying and interacting with SQL resultsets."
  (setq-local scoot-local--table-name-resolvers '(scoot-rs--tables-in-result
                                                  scoot-connection--table-prompt))

  (setq-local scoot-xref-action-fn 'scoot-rs--xref-action)
  (setq-local scoot-xref-identifier-at-point-fn 'scoot-rs--xref-id-at-point)
  (setq-local scoot-xref-completion-provider-fn 'scoot-rs--xref-completions)
  (add-hook 'xref-backend-functions #'scoot--xref-backend nil t)

  (scoot-table--ensure-row-mark-table))



(provide 'scoot-resultset)

;;; scoot-resultset.el ends here
