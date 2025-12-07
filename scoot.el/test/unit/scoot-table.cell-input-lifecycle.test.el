;;; scoot-table.cell-input-lifecycle.test.el --- summary -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-parametrized)
(require 'scoot-table)
(require 'scoot-test-fixtures)
(require 'scoot-nexartrade-fixtures)



;; install input

(ert-parametrized-deftest scoot-table--scoot-input--install-input!
    (column-index
     expected-widget-region
     expected-editable-region
     expected-align)
    (("column:id"
      (:eval 0)
      (:quote (554 . 558))
      (:quote (557 . 558))
      (:quote right))
     ("column:username"
      (:eval 1)
      (:quote (561 . 577))
      (:quote (561 . 577))
      (:quote left)))
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data t)
   (goto-char (point-min))
   (scoot-table--move-to-first-row!)
   (dotimes (_ (1+ column-index))
     (scoot-table--cell-right!))

   ;; When
   (let ((input (scoot-table--edit-cell)))
     (should input)
     (should (equal (plist-get input :widget-start-line) 4))
     (should (equal (plist-get input :align) expected-align))
     (should (equal (scoot-widget--region input)
                    expected-widget-region))
     (should (equal (scoot-widget--editable-region input)
                    expected-editable-region))
     (should (plist-get input :contain-cursor)))))


;; refresh-input

(ert-parametrized-deftest scoot-table--scoot-input--refresh-input!--original-value
    (cell-index)
    (("column:id--row:0"
      (:quote (0 . 0)))
     ("column:username--row:0"
      (:quote (1 . 0)))
     ("column:username--row:1"
      (:quote (1 . 1))))
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data t)
   (goto-char (point-min))
   (scoot-table--move-to-first-row!)
   (dotimes (_ (1+ (car cell-index)))
     (scoot-table--cell-right!))
   (dotimes (_ (cdr cell-index))
     (scoot-table--cell-down!))

   (let ((input (scoot-table--edit-cell))
         expected-widget-region
         expected-editable-region
         expected-content)
     (should input)

     (setq expected-widget-region (scoot-widget--region input))
     (setq expected-editable-region (scoot-widget--editable-region input))
     (setq expected-content (scoot-widget--buffer-string input))

     ;; When
     (scoot-input--refresh-input!)

     ;; Then
     (should (equal (plist-get input :widget-start-line) (+ 4 (cdr cell-index))))
     (should (equal (scoot-widget--region input)
                    expected-widget-region))
     (should (equal (scoot-widget--editable-region input)
                    expected-editable-region))
     (should (equal (scoot-widget--buffer-string input)
                    expected-content)))))

(ert-parametrized-deftest scoot-table--scoot-input--refresh-input!
    (column-index
     new-value
     expected-widget-region
     expected-editable-region
     expected-align)
    (("column:id--len-2"
      (:eval 0)
      (:eval "10")
      (:quote (554 . 558))
      (:quote (556 . 558))
      (:quote right))
     ("column:username--len-15"
      (:eval 1)
      (:eval "tom_atojuicesso")
      (:quote (561 . 577))
      (:quote (561 . 576))
      (:quote left))
     ("column:username--len-17"
      (:eval 1)
      (:eval "tom_atojuicesson2")
      (:quote (561 . 578))
      (:quote (561 . 578))
      (:quote left)))
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data t)
   (goto-char (point-min))
   (scoot-table--move-to-first-row!)
   (dotimes (_ (1+ column-index))
     (scoot-table--cell-right!))

   ;; When
   (let ((input (scoot-table--edit-cell)))
     (should input)

     (with-scoot-widget-shadow-buffer input
       (with-silent-modifications
         (erase-buffer))
       (insert new-value))
     (scoot-input--refresh-input!)

     ;; Then
     (should (equal (plist-get input :widget-start-line) 4))
     (should (equal (plist-get input :align) expected-align))
     (should (equal (scoot-widget--region input)
                    expected-widget-region))
     (should (equal (scoot-widget--editable-region input)
                    expected-editable-region))
     (should (plist-get input :contain-cursor)))))



;; remove cell editor

(ert-parametrized-deftest scoot-table--scoot-input--remove-cell-editor!
    (cell-index
     content-with-border)
    (("column:id--row:0"
      (:quote (0 . 0))
      (:eval "|    1 |"))
     ("column:username--row:0"
      (:quote (1 . 0))
      (:eval "| tom_atojuicesson |")))
  (with-new-window-buffer
   ;; Given
   (let ((table (scoot-table--insert-table! nexartrade-table--users--result-data t)))
     (goto-char (point-min))
     (scoot-table--move-to-first-row!)
     (dotimes (_ (1+ (car cell-index)))
       (scoot-table--cell-right!))
     (dotimes (_ (cdr cell-index))
       (scoot-table--cell-down!))

     ;; When
     (let* ((cell (scoot-table--cell-at-point))
            (cell-bounds (cons (- (car (scoot-table--cell-begin)) 2)
                               (+ (car (scoot-table--cell-end)) 3)))
            (_precond (should (equal (buffer-substring-no-properties (car cell-bounds)
                                                                    (cdr cell-bounds))
                                    content-with-border)))
            (input (scoot-table--edit-cell)))
       (should input)
       (scoot-table--remove-cell-editor! table input cell)

       ;; Then
       (should (equal (buffer-substring-no-properties (car cell-bounds)
                                                      (cdr cell-bounds))
                      content-with-border))))))



;;; scoot-table.cell-input-lifecycle.test.el ends here
