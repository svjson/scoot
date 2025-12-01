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
(require 'ert)



;; Edit string/varchar cell

(ert-deftest scoot-table--cell-editing--edit-varchar-cell ()
  ;; Given
  (let ((result-data '((columns . ["id" "username"])
                       (rows . [[7 "barb_dwyer"] [2 "ben_rangel"]])
                       (metadata
                        (columns
                         . [((name . "id") (type . "INTEGER")
                             (typespec (type . "INTEGER") (bits . 64) (signed . t))
                             (native_type . "int") (nullable . :json-false)
                             (primary_key . t) (default) (table . "users") (column . "id")
                             (constraints . []))
                            ((name . "username") (type . "STRING")
                             (typespec (type . "STRING") (max-len . 50) (encoding . "utf-8")
                                       (collation (locale . "Latin1_General_CP1")
                                                  (case-sensitive . :json-false)
                                                  (accent-sensitive . t))
                                       (lob . :json-false))
                             (native_type
                              . "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS")
                             (nullable . t) (primary_key . :json-false) (default)
                             (table . "users") (column . "username") (constraints . []))])))))

    (with-new-window-buffer
     (scoot-table--insert-table! result-data t)
     (should
      (equal
       (buffer-substring-no-properties (point-min) (point-max))
       (string-join
        '("+------+------------+"
          "| 🔑id | username   |"
          "+------+------------+"
          "|    7 | barb_dwyer |"
          "|    2 | ben_rangel |"
          "+------+------------+"
          "")
        "\n")))

     (goto-char (point-min))
     (scoot-table--move-to-last-row!)
     (scoot-table--move-to-last-column)

     (message "%s" (scoot--props-at-point))
     (call-interactively #'scoot-table--edit-cell)

     (should (bound-and-true-p scoot-input-mode))

     (dotimes (_ 6)
       (do-command #'delete-backward-char))

     (interactively-self-insert-char ?d)
     (interactively-self-insert-char ?o)
     (interactively-self-insert-char ?v)
     (interactively-self-insert-char ?e)
     (interactively-self-insert-char ?r)

     (should
      (equal
       (buffer-substring-no-properties (point-min) (point-max))
       (string-join
        '("+------+------------+"
          "| 🔑id | username   |"
          "+------+------------+"
          "|    7 | barb_dwyer |"
          "|    2 | ben_dover  |"
          "+------+------------+"
          "")
        "\n")))

     (do-command #'scoot-input--confirm-edit)

     (should
      (equal
       (buffer-substring-no-properties (point-min) (point-max))
       (string-join
        '("+------+------------+"
          "| 🔑id | username   |"
          "+------+------------+"
          "|    7 | barb_dwyer |"
          "|    2 |>ben_dover  |"
          "+------+------------+"
          "")
        "\n"))))))




;;; scoot-table.cell-editing.test.el ends here
