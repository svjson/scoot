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

(defvar-local scoot-result--result-data nil
  "The result data model for the current Scoot Result buffer.")

(defvar-local scoot-result--result-model nil
  "The model backing the visual representation of the data set.")

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
     ((string-equal "INTEGER" column-type) scoot-formatter-number)
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

(defun scoot-result--build-initial-buffer-model (query-result)
  "Construct the intial buffer-backing model from QUERY-RESULT."
  query-result)

(defun scoot-result--insert-buffer-info ()
  "Insert result buffer basic header information."
  (scoot--insert-faced "Connection: " 'scoot-label-face)
  (insert scoot-result--result-connection-name)
  (insert "\n"))

(defun scoot-result-refresh-buffer ()
  "Redraw the entire buffer from scoot-result--result-model."
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (scoot-result--insert-buffer-info)
  (insert "\n")

  (scoot--insert-faced "Query: " 'scoot-label-face)
  (insert "\n")
  (scoot--insert-query-box (format "%s" scoot-result--current-sql-statement))
  (insert "\n")

  (scoot-result--insert-result-set)

  (read-only-mode 1))

(defun scoot--open-result-buffer (result connection stmt)
  "Open a Scoot Result Buffer using a RESULT, CONNECTION and STMT."

  ;; FIXME: Decide on a scheme to uniquely identify result buffers
  (let ((buf (get-buffer-create "*scoot-result*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (scoot-result-mode)
      (setq-local scoot-result--result-connection-name connection
                  scoot-result--original-sql-statement stmt
                  scoot-result--current-sql-statement stmt
                  scoot-result--result-data result)
      (scoot-result-refresh-buffer)
      (display-buffer buf))))

(defvar scoot-result-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'scoot-result-refresh-buffer)
    map)
  "Keymap for `scoot-result-mode`.")

(define-derived-mode scoot-result-mode special-mode "Scoot Result"
  "Major mode for displaying and interacting with SQL resultsets."

  (setq-local truncate-lines t)
  (setq buffer-read-only t))

(provide 'scoot-result)

;;; scoot-result.el ends here
