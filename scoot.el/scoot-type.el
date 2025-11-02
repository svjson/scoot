;;; scoot-type.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(defun scoot--value-to-string (val &rest _)
  "Stringify a value, VAL, for display in a result set table."
  (if (null val)
      "NULL"
    (format "%s" val)))

(defun scoot--format-temporal (val &rest _)
  "Format a temporal value, VAL, for display in a result set table."
  (let ((str-val (scoot--value-to-string val)))
    (if (null val)
        str-val
      (replace-regexp-in-string "[+-][0-1][0-9]:[0-6][0-9]" "" str-val))))

(defvar scoot-formatter-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (insert (propertize formatted-value 'face 'scoot-cell-string-face)))
        :sql-literal
        (lambda (value)
          (concat "'" value "'"))))

(defvar scoot-formatter-number
  (list :align 'right
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (insert (propertize formatted-value 'face 'scoot-cell-number-face)))
        :sql-literal
        #'scoot--value-to-string))

(defvar scoot-formatter-boolean
  (list :align 'right
        :format-value (lambda (val &rest _)
                        (if (eq val t) "true" "false"))
        :output-cell
        (lambda (val formatted-value)
          (insert (propertize formatted-value 'face
                              (if (eq val t)
                                  'scoot-cell-boolean-true-face
                                'scoot-cell-boolean-false-face))))
        (lambda (value)
          (cond
           (value "TRUE")
           (t "FALSE")))))

(defvar scoot-formatter-temporal
  (list :align 'left
        :format-value #'scoot--format-temporal
        :output-cell
        (lambda (_ formatted-value)
          (insert (propertize formatted-value 'face 'scoot-cell-temporal-face)))
        :sql-literal
        (lambda (value)
          (concat "'" value "'"))))

(defvar scoot-formatter-raw-string
  (list :align 'left
        :format-value #'scoot--value-to-string
        :output-cell
        (lambda (_ formatted-value)
          (insert (propertize formatted-value 'type 'scoot-cell-generic-face)))
        :sql-literal
        (lambda (value)
          (concat "'" value "'"))))

(defvar scoot-formatter-null
  (list :align 'right
        :format-value (lambda (_) "NULL")
        :output-cell
        (lambda (_ formatted-value)
          (insert (propertize formatted-value 'type 'scoot-cell-null-face)))
        :sql-literal
        (lambda (_) "NULL")))

(defun scoot--resolve-formatter-from-column-metadata (column)
  "Resolve cell formatter based on COLUMN type."
  (let ((column-type (alist-get 'type column)))
    (cond
     ((equal "STRING" column-type) scoot-formatter-string)
     ((equal "OBJECT-NAME" column-type) scoot-formatter-string)
     ((equal "INTEGER" column-type) scoot-formatter-number)
     ((equal "DECIMAL" column-type) scoot-formatter-number)
     ((equal "BOOLEAN" column-type) scoot-formatter-boolean)
     ((equal "TEMPORAL" column-type) scoot-formatter-temporal)
     (t scoot-formatter-raw-string))))

(defun scoot--format-value (formatter value &optional metadata)
  "Format VALUE with FORMATTER using :format-value.

Optionally provide column METADATA."
  (funcall (plist-get formatter :format-value)
           value
           metadata))

(defun scoot--format-literal (formatter value)
  "Format VALUE as a literal using FORMATTER."
  (funcall (plist-get formatter :sql-literal) value))

(provide 'scoot-type)

;;; scoot-type.el ends here
