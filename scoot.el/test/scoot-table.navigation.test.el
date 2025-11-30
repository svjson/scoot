;;; scoot-table.navigation.test.el --- summary -*- lexical-binding: t -*-

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

(require 'scoot-table)
(require 'scoot-test-fixtures)
(require 'scoot-nexartrade-fixtures)
(require 'ert)


;; scoot-table--move-to-first-row

(ert-deftest scoot-table--move-to-first-row--from-table-start ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))

   ;; When
   (scoot-table--move-to-first-row)

   ;; Then
   (should (equal (line-number-at-pos) 4))
   (should (equal (current-column) 0))))

(ert-deftest scoot-table--move-to-first-row--from-first-table-header ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (forward-line 1)
   (forward-char 2)

   ;; When
   (scoot-table--move-to-first-row)

   ;; Then
   (should (equal (line-number-at-pos) 4))
   (should (equal (current-column) 5))))

(ert-deftest scoot-table--move-to-first-row--from-table-row-3-column-1 ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (forward-line 6)
   (forward-char 2)

   ;; When
   (scoot-table--move-to-first-row)

   ;; Then
   (should (equal (line-number-at-pos) 4))
   (should (equal (current-column) 5))))




;; scoot-table--move-to-last-row

(ert-deftest scoot-table--move-to-last-row--from-table-start ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))

   ;; When
   (scoot-table--move-to-last-row)

   ;; Then
   (should (equal (line-number-at-pos) 13))
   (should (equal (current-column) 0))))

(ert-deftest scoot-table--move-to-last-row--from-first-table-header ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (forward-line 1)
   (forward-char 2)

   ;; When
   (scoot-table--move-to-last-row)

   ;; Then
   (should (equal (line-number-at-pos) 13))
   (should (equal (current-column) 5))))

(ert-deftest scoot-table--move-to-last-row--from-table-row-3-column-1 ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (forward-line 6)
   (forward-char 2)

   ;; When
   (scoot-table--move-to-last-row)

   ;; Then
   (should (equal (line-number-at-pos) 13))
   (should (equal (current-column) 5))))



;; scoot-table--next-cell

(ert-deftest scoot-table--next-cell--from-table-start ()
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))

   ;; Then
   (should (equal (scoot-table--next-cell)
                  (cons 186 2)))))




;;; scoot-table.navigation.test.el ends here
