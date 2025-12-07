;;; scoot-table.resize-column.test.el --- summary -*- lexical-binding: t -*-

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



;; resize id column

(ert-parametrized-deftest scoot-table--scoot-table--resize-column!
    (column-index
     to-width
     expected-table)
    (("column:id--to-width-5"
      (:eval 0)
      (:eval 5)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:id--to-width-6"
      (:eval 0)
      (:eval 6)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:id--to-width-7"
      (:eval 0)
      (:eval 7)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:id--to-width-8"
      (:eval 0)
      (:eval 8)
      (:quote ("+--------+------------+"
               "| PK id  | username   |"
               "+--------+------------+"
               "|      7 | barb_dwyer |"
               "|      2 | ben_rangel |"
               "+--------+------------+")))
     ("column:id--to-width-9"
      (:eval 0)
      (:eval 9)
      (:quote ("+--------+------------+"
               "| PK id  | username   |"
               "+--------+------------+"
               "|      7 | barb_dwyer |"
               "|      2 | ben_rangel |"
               "+--------+------------+")))
     ("column:id--to-width-10"
      (:eval 0)
      (:eval 10)
      (:quote ("+---------+------------+"
               "| PK id   | username   |"
               "+---------+------------+"
               "|       7 | barb_dwyer |"
               "|       2 | ben_rangel |"
               "+---------+------------+")))
     ("column:id--to-width-11"
      (:eval 0)
      (:eval 11)
      (:quote ("+----------+------------+"
               "| PK id    | username   |"
               "+----------+------------+"
               "|        7 | barb_dwyer |"
               "|        2 | ben_rangel |"
               "+----------+------------+")))
     ("column:username--to-width-0"
      (:eval 0)
      (:eval 0)
      (:quote ("+----------+------------+"
               "| PK id    | username   |"
               "+----------+------------+"
               "|        7 | barb_dwyer |"
               "|        2 | ben_rangel |"
               "+----------+------------+")))
     ("column:username--to-width-8"
      (:eval 0)
      (:eval 8)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:username--to-width-9"
      (:eval 0)
      (:eval 9)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:username--to-width-10"
      (:eval 0)
      (:eval 10)
      (:quote ("+-------+------------+"
               "| PK id | username   |"
               "+-------+------------+"
               "|     7 | barb_dwyer |"
               "|     2 | ben_rangel |"
               "+-------+------------+")))
     ("column:username--to-width-11"
      (:eval 0)
      (:eval 11)
      (:quote ("+--------+-------------+"
               "| PK id  | username    |"
               "+--------+-------------+"
               "|      7 | barb_dwyer  |"
               "|      2 | ben_rangel  |"
               "+--------+-------------+")))
     ("column:username--to-width-12"
      (:eval 0)
      (:eval 12)
      (:quote ("+--------+--------------+"
               "| PK id  | username     |"
               "+--------+--------------+"
               "|      7 | barb_dwyer   |"
               "|      2 | ben_rangel   |"
               "+--------+--------------+")))
     ("column:username--to-width-13"
      (:eval 0)
      (:eval 13)
      (:quote ("+---------+---------------+"
               "| PK id   | username      |"
               "+---------+---------------+"
               "|       7 | barb_dwyer    |"
               "|       2 | ben_rangel    |"
               "+---------+---------------+"))))
  (with-alphanum-keys
   (with-new-window-buffer
    ;; Given
    (let ((table (scoot-table--insert-table! (scoot-test--result-data
                                              :data nexartrade-table--users--result-data
                                              :columns ["id" "username"]
                                              :rows [6 1])
                                             t)))
      (goto-char (point-min))

      ;; When
      (scoot-table--resize-column! table 0 to-width)
      (equal
       (buffer-substring-no-properties (point-min) (point-max))
       (string-join
        (append expected-table '(""))
        "\n"))))))




;;; scoot-table.resize-column.test.el ends here
