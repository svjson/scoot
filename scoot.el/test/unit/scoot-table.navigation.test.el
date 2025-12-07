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

(require 'ert)
(require 'ert-parametrized)
(require 'scoot-table)
(require 'scoot-test-fixtures)
(require 'scoot-nexartrade-fixtures)



;; scoot-table--move-to-first-row!

(ert-parametrized-deftest scoot-table--move-to-first-row!
    (move-to-initial-pos expected-pos)

    (("from-table-start"
      (:fun (goto-char (car  (scoot-table--table-start))))
      (:quote (4 . 0)))

     ("from-first-table-header"
      (:fun (progn (forward-line 1) (forward-char 2)))
      (:quote (4 . 5)))

     ("from-table-row-3-column-1"
      (:fun (progn (forward-line 6) (forward-char 2)))
      (:eval (cons 4 5))))
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (move-to-initial-pos)

   ;; When
   (scoot-table--move-to-first-row!)

   ;; Then
   (should (equal (line-number-at-pos) (car expected-pos)))
   (should (equal (current-column) (cdr expected-pos)))))




;; scoot-table--move-to-last-row!

(ert-parametrized-deftest scoot-table--move-to-last-row!
    (move-to-initial-pos! expected-line expected-column)
    (("from-table-start"
       (:fun (goto-char (car (scoot-table--table-start))))
       (:eval 13)
       (:eval 0))
     ("from-first-table-header"
      (:fun (progn (forward-line 1) (forward-char 2)))
      (:eval 13)
      (:eval 5))
     ("from-table-row-3-column-1"
      (:fun (progn (forward-line 6) (forward-char 2)))
      (:eval 13)
      (:eval 5)))
  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (move-to-initial-pos!)

   ;; When
   (scoot-table--move-to-last-row!)

   ;; Then
   (should (equal (line-number-at-pos)
                  expected-line))
   (should (equal (current-column)
                  expected-column))))



;; scoot-table--cell-right!

(ert-parametrized-deftest scoot-table--cell-right!
    (move-to-initial-pos! expected-next-cell-pos expected-thing)

    (("from-table-start"
      (:fun (goto-char (car (scoot-table--table-start))))
      (:quote (187 . 2))
      (:quote table-header))
     ("from-first-table-header"
      (:fun (goto-char 187))
      (:quote (193 . 2))
      (:quote table-header))
     ("from-last-table-header"
      (:fun (goto-char 357))
      (:quote (557 . 4))
      (:quote table-cell)))

  (with-new-window-buffer
   ;; Given
   (scoot-table--insert-table! nexartrade-table--users--result-data)
   (goto-char (point-min))
   (move-to-initial-pos!)

   ;; When
   (scoot-table--cell-right!)

   ;; Then
   (should (equal (cons (point) (line-number-at-pos))
                  expected-next-cell-pos))
   (should (equal (scoot--thing-at)
                  expected-thing))))




;;; scoot-table.navigation.test.el ends here
