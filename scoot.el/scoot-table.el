;;; scoot-table.el --- summary -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'scoot-common)
(require 'scoot-type)


;; Customization Options

(defcustom scoot-primary-key-icon "🔑"
  "Icon used to indicate that a column has a primary key constraint."
  :type 'string
  :group 'scoot)

(defcustom scoot-foreign-key-icon "🗝️"
  "Icon used to indicate that a column has a foreign key constraint."
  :type 'string
  :group 'scoot)



;; Variables

(defvar-local scoot-table--table-model nil
  "The model backing the table representation of the data set in.")



;; Formatting

(defvar scoot-formatter-header
  (list :align 'left
        :format-value (lambda (name metadata)
                        (format "%s%s"
                                (cond ((eq t (alist-get 'primary_key metadata))
                                       scoot-primary-key-icon)

                                      ((cl-some
                                        (lambda (con)
                                          (equal (alist-get 'type con) "fk"))
                                        (alist-get 'constraints metadata))
                                       scoot-foreign-key-icon)

                                      (t ""))
                                name))
        :output-cell (lambda (_ formatted-value)
                       (insert (propertize formatted-value
                                           'face 'scoot-header-face)))))



;; Table Model

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

(defun scoot-table--refresh-visual-model (result-data)
  "Build the table data model from RESULT-DATA."
  (let* ((columns-metadata (alist-get 'columns
                                      (alist-get 'metadata
                                                 result-data)))
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
         (raw-table-data (alist-get 'rows result-data))
         (records (cl-mapcar
                   (lambda (row)
                     (cl-mapcar (lambda (cell-value fmt col)
                                  (let ((formatter (if (null cell-value)
                                                       scoot-formatter-null
                                                     fmt)))
                                    (list :value (cond
                                                  ;; Temporary ugly, no good, verybad hack to standardize date output between backends
                                                  ((and cell-value
                                                        (member (alist-get 'native_type col) '("datetimeoffset" "DATETIMEOFFSET" "TIMESTAMP" "TIMESTAMP WITH TIME ZONE")))
                                                   (scoot--format-temporal cell-value))
                                                  (t cell-value))
                                          :formatted-value
                                          (funcall
                                           (plist-get formatter :format-value)
                                           cell-value))))
                                row
                                formatters
                                columns-metadata))
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
    (setq scoot-table--table-model
          (list :headers headers
                :tables tables
                :widths widths
                :formatters formatters
                :records records))))



;; Table Rendering


(defun scoot-table--insert-table-header ()
  "Insert the result set table header."
  (scoot-table--insert-divider-row)

  (cl-mapc (lambda (header width)
             (insert (propertize "| "
                                 'thing 'table-border
                                 'face 'scoot-table-face))
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
                                                               'column-meta (plist-get header :metadata)
                                                               'column (alist-get 'column (plist-get header :metadata))
                                                               'table (alist-get 'table (plist-get header :metadata))))))
           (plist-get scoot-table--table-model :headers)
           (plist-get scoot-table--table-model :widths))
  (insert (propertize "|"
                      'thing 'table-border
                      'face 'scoot-table-face))
  (insert "\n")

  (scoot-table--insert-divider-row))

(defun scoot-table--insert-table-row (row)
  "Insert the table row ROW."

  (cl-mapc (lambda (header cell width fmt)
             (insert (propertize
                      "| "
                      'thing 'table-border
                      'face 'scoot-table-face))
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
           (plist-get scoot-table--table-model :headers)
           row
           (plist-get scoot-table--table-model :widths)
           (plist-get scoot-table--table-model :formatters))
  (insert (propertize "|"
                      'thing 'table-border
                      'face 'scoot-table-face))
  (insert "\n"))


(defun scoot-table--insert-divider-row ()
  "Insert a horizontal table divider."
  (insert (propertize
           (concat "+-"
                   (mapconcat (lambda (w) (make-string w ?-))
                              (plist-get scoot-table--table-model :widths)
                              "-+-")
                   "-+\n")
           'thing 'table-border
           'face 'scoot-table-face)))

(defun scoot-table--insert-table (result-data)
  "Insert the RESULT-DATA table into the buffer."
  (scoot-table--refresh-visual-model result-data)
  (scoot-table--insert-table-header)
  (mapc #'scoot-table--insert-table-row
        (plist-get scoot-table--table-model :records))
  (scoot-table--insert-divider-row))



;; Table inspection

(defun scoot-table--table-at-point-p ()
  "Return non-nil if the cursor is inside the table bounds."
  (scoot--thing-at-p (point) '(table-cell
                               table-header
                               table-border)))

(defun scoot-table--cell-at-point ()
  "Return the result set table cell at point."
  (let* ((props (scoot--props-at-point)))
    (list :type (alist-get 'thing props)
          :column (alist-get 'column-meta props)
          :value (alist-get 'value props)
          :formatter (alist-get 'formatter props))))

(defun scoot-table--cell-begin (&optional point)
  "Find the location of the first char of the cell at POINT."
  (let ((p (or point (point))))
    (when (scoot--thing-at-p p '(table-cell table-header))
      (while (scoot--thing-at-p (1- p) '(table-cell table-header))
        (setq p (1- p)))
      (setq p (1+ p))
      (cons p (line-number-at-pos p)))))

(defun scoot-table--cell-end (&optional point)
  "Find the location of the first char of the cell at POINT."
  (let ((p (or point (point))))
    (when (scoot--thing-at-p p '(table-cell table-header))
      (setq p (next-single-property-change point 'thing))
      (when p
        (setq p (- p (if (not (scoot--thing-at-p p '(table-cell table-header))) 2 1)))
        (cons p (line-number-at-pos p))))))


(defun scoot-table--next-cell (&optional point)
  "Find the location of the the next table-cell of the buffer.

If POINT is not provided, the search will start from the beginning
of the buffer."
  (save-excursion
    (when point (goto-char point))
    (when-let (point (scoot--next-property-with-value-in
                      'thing
                      '(table-cell table-header)))
      (goto-char point)
      (cons point (line-number-at-pos)))))

(defun scoot-table--previous-cell (&optional point)
  "Find the location of the the previous table-cell of the buffer.

If POINT is not provided, the search will start from the end of the
buffer."
  (save-excursion
    (when point (goto-char point))
    (when-let (point (scoot--previous-property-with-value-in
                      'thing
                      '(table-cell table-header)))
      (goto-char point)
      (cons point (line-number-at-pos)))))



;; Table Navigation

(defun scoot-table--move-to-cell-value ()
  "Move the cursor to the align anchor of the cell value."
  (if (or (equal (plist-get (alist-get 'formatter (scoot--props-at-point)) :align) 'left)
          (not (car (assoc 'value (scoot--props-at-point)))))
      (goto-char (car (scoot-table--cell-begin (point))))
    (goto-char (car (scoot-table--cell-end (point))))))

(defun scoot-table--cell-right ()
  "Move right to the next cell."
  (interactive)
  (when-let (cell (scoot-table--next-cell (point)))
    (goto-char (car cell))
    (scoot-table--move-to-cell-value)))

(defun scoot-table--cell-left ()
  "Move right to the previous cell."
  (interactive)
  (when-let (cell (scoot-table--previous-cell (point)))
    (goto-char (car cell))
    (scoot-table--move-to-cell-value)))

(defun scoot-table--cell-up ()
  "Move up to the corresponding cell in the previous row."
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively 'previous-line)
    (when (scoot--thing-at-p (point) '(table-cell table-header))
      (scoot-table--move-to-cell-value))))

(defun scoot-table--cell-down ()
  "Move up to the corresponding cell in the previous row."
  (interactive)
  (let ((line-move-visual nil))
    (call-interactively 'next-line)
    (when (scoot--thing-at-p (point) '(table-cell table-header))
      (scoot-table--move-to-cell-value))))


;; Scoot Table Mode - scoot-table--mode

(defvar scoot-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'scoot-table--cell-down)
    (define-key map (kbd "C-p") 'scoot-table--cell-up)
    (define-key map (kbd "C-f") 'scoot-table--cell-right)
    (define-key map (kbd "TAB") 'scoot-table--cell-right)
    (define-key map (kbd "C-b") 'scoot-table--cell-left)
    (define-key map (kbd "<backtab>") 'scoot-table--cell-left)
    map))

(define-minor-mode scoot-table-mode
  "Minor mode for navigating and interacting with tables."
  :lighter " Table"
  :keymap scoot-table-mode-map)


(provide 'scoot-table)

;;; scoot-table.el ends here
