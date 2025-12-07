;;; scoot-table.row-marks.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-buffer)
(require 'scoot-table)
(require 'scoot-test-fixtures)
(require 'scoot-test-table-fixtures)
(require 'scoot-nexartrade-fixtures)



;; Mark types

(ert-deftest scoot-table--row-marks--edit-cell-value-adds-row-mark ()
  (with-new-window-buffer
   ;; Given
   (let ((table (scoot-table--insert-table! nexartrade-table--users--result-data t)))

     ;; When
     (scoot-test-table--edit-cell! table '(1 . 2) "ben.rangel@example.se")

     ;; Then
     (let ((row-mark (scoot-table--get-row-mark table '(:id 2))))
       (should (overlayp row-mark))
       (should (equal (overlay-get row-mark 'type)
                      :update))))))

(ert-deftest scoot-table--row-marks--confirm-edit-with-no-change-does-not-add-row-mark ()
  (with-new-window-buffer
   ;; Given
   (let ((table (scoot-table--insert-table! nexartrade-table--users--result-data t)))

     ;; When
     (scoot-test-table--move-to-cell! table '(1 . 2))
     (do-command #'scoot-table--edit-cell)
     (do-command #'scoot-input--confirm-edit)

     ;; Then
     (should (null (scoot-table--get-row-mark table '(:id 2)))))))

(ert-deftest scoot-table--row-marks--restore-buffer-removes-row-mark ()
  (with-new-window-buffer
   ;; Given
   (let ((table (scoot-table--insert-table! nexartrade-table--users--result-data t)))

     (scoot-test-table--edit-cell! table '(1 . 2) "here.lives.dog@example.com")
     (let ((row-mark (scoot-table--get-row-mark table '(:id 2))))
       (should (overlayp row-mark))
       (should (equal (overlay-get row-mark 'type)
                      :update)))

     ;; When
     (do-command #'scoot-buffer-refresh)

     ;; Then
     (should (null (scoot-table--get-row-mark table '(:id 2)))))))



;;; scoot-table.row-marks.test.el ends here
