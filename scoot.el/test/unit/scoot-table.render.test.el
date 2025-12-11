;;; scoot-table.render.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-test-buffer-fixtures)
(require 'scoot-test-table-fixtures)
(require 'scoot-test-customization-fixtures)
(require 'scoot-nexartrade-fixtures)
(require 'ert)


;; scoot-table--insert-table!

(ert-deftest scoot-table--insert-table!--table-from-result-data ()
  ;; Given
  (let ((result-data '((columns . ["RentalObjectCode" "Status"])
                        (rows . [["705-021-99-0008" 3]
                                 ["900-001-01-0001" 2]
                                 ["100-009-18-0024" 3]])
                        (metadata
                         (columns
                          . [((name . "RentalObjectCode") (type . "STRING")
                              (typespec (type . "STRING") (max-len . 255)
                                        (encoding . "utf-8")
                                        (collation (locale . "Finnish_Swedish")
                                                   (case-sensitive . :json-false)
                                                   (accent-sensitive . t))
                                        (lob . :json-false))
                              (native_type . "nvarchar(255) COLLATE Finnish_Swedish_CI_AS")
                              (nullable . :json-false) (primary_key . :json-false) (default)
                              (table . "Listing") (column . "RentalObjectCode")
                              (constraints . []))
                             ((name . "Status") (type . "INTEGER")
                              (typespec (type . "INTEGER") (bits . 64) (signed . t))
                              (native_type . "int") (nullable . :json-false)
                              (primary_key . :json-false) (default) (table . "Listing")
                              (column . "Status") (constraints . []))])))))

    (with-new-window-buffer
     (insert "\n\n")
     (scoot-table--insert-table! result-data)
     (should
      (equal
       (buffer-substring-no-properties (point-min) (point-max))
       (string-join
        '(""
          ""
          "+------------------+--------+"
          "| RentalObjectCode | Status |"
          "+------------------+--------+"
          "| 705-021-99-0008  |      3 |"
          "| 900-001-01-0001  |      2 |"
          "| 100-009-18-0024  |      3 |"
          "+------------------+--------+"
          "")
        "\n"))))))




;;; scoot-table.render.test.el ends here
