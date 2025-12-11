;;; scoot-table.cell-editing.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-test-table-fixtures)
(require 'scoot-nexartrade-fixtures)
(require 'ert)



;; Edit string/varchar cell

(ert-deftest scoot-table--cell-editing--varchar-cell ()
  ;; Given
  (let ((result-data (scoot-test--result-data
                      :data nexartrade-table--users--result-data
                      :columns ["id" "username"]
                      :rows [6 1])))
    (with-alphanum-keys
     (with-new-window-buffer
      (let ((table (scoot-table--insert-table! result-data t)))
        (should
         (equal
          (scoot-widget--buffer-string table)
          (string-join
           '("+-------+------------+"
             "| PK id | username   |"
             "+-------+------------+"
             "|     7 | barb_dwyer |"
             "|     2 | ben_rangel |"
             "+-------+------------+"
             "")
           "\n")))

        ;; When - input content is edited
        (scoot-test-table--edit-cell! table '(1 . 2) "ben_dover" t)

        ;; Then - table matches input
        (should
         (equal
          (scoot-widget--buffer-string table)
          (string-join
           '("+-------+------------+"
             "| PK id | username   |"
             "+-------+------------+"
             "|     7 | barb_dwyer |"
             "|     2 | ben_dover  |"
             "+-------+------------+"
             "")
           "\n")))

        ;; When - input is confirmed
        (do-command #'scoot-input--confirm-edit)

        ;; Then - table shows content and modified marker
        (should
         (equal
          (scoot-widget--buffer-string table)
          (string-join
           '("+-------+------------+"
             "| PK id | username   |"
             "+-------+------------+"
             "|     7 | barb_dwyer |"
             "|     2 |>ben_dover  |"
             "+-------+------------+"
             "")
           "\n"))))))))

(ert-deftest scoot-table--cell-editing--last-column--varchar--column-expansion ()
  ;; Given
  (let ((result-data (scoot-test--result-data
                      :data nexartrade-table--users--result-data
                      :columns ["id" "username"]
                      :rows [6 1])))
    (with-alphanum-keys
     (with-new-window-buffer
      (let ((table (scoot-table--insert-table! result-data t)))
        (should
         (equal
          (scoot-widget--buffer-string table)
          (string-join
           '("+-------+------------+"
             "| PK id | username   |"
             "+-------+------------+"
             "|     7 | barb_dwyer |"
             "|     2 | ben_rangel |"
             "+-------+------------+"
             "")
           "\n")))

        ;; When - a character is added
        (scoot-test-table--move-to-cell! table '(1 . 2))
        (scoot-table--edit-cell)
        (interactively-self-insert-char ?s)

        ;; Then - last column has been expanded by one
        (should
         (equal
          (scoot-widget--buffer-string table)
          (string-join
           '("+-------+-------------+"
             "| PK id | username    |"
             "+-------+-------------+"
             "|     7 | barb_dwyer  |"
             "|     2 | ben_rangels |"
             "+-------+-------------+"
             "")
           "\n"))))))))




;;; scoot-table.cell-editing.test.el ends here
