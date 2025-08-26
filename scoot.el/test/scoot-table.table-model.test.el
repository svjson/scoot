;;; scoot-test.el --- summary -*- lexical-binding: t -*-

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

(require 'scoot)
(require 'scoot-table)
(require 'scoot-common)
(require 'ert)

(ert-deftest scoot-table--build-visual-model--simple-example ()
  (let ((result-model '((columns . ["RentalObjectCode" "Status"])
                        (rows . [["705-021-99-0008" 3]])
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
    (should
     (equal
      (scoot-table--build-visual-model result-model)
      '(:headers
        ((:name "RentalObjectCode" :header-label "RentalObjectCode" :metadata ((name . "RentalObjectCode") (type . "STRING") (typespec (type . "STRING") (max-len . 255) (encoding . "utf-8") (collation (locale . "Finnish_Swedish") (case-sensitive . :json-false) (accent-sensitive . t)) (lob . :json-false)) (native_type . "nvarchar(255) COLLATE Finnish_Swedish_CI_AS") (nullable . :json-false) (primary_key . :json-false) (default) (table . "Listing") (column . "RentalObjectCode") (constraints . [])))
         (:name "Status" :header-label "Status" :metadata ((name . "Status") (type . "INTEGER") (typespec (type . "INTEGER") (bits . 64) (signed . t)) (native_type . "int") (nullable . :json-false) (primary_key . :json-false) (default) (table . "Listing") (column . "Status") (constraints . []))))
        :tables ("Listing")
        :widths (16 6)
        :formatters
        ((:align left :format-value scoot--value-to-string :output-cell #[(_ formatted-value) ((insert (propertize formatted-value 'face 'scoot-cell-string-face))) (t)] :sql-literal #[(value) ((concat "'" value "'")) (t)])
         (:align right :format-value scoot--value-to-string :output-cell #[(_ formatted-value) ((insert (propertize formatted-value 'face 'scoot-cell-number-face))) (t)] :sql-literal identity))
        :records
        (((:value "705-021-99-0008" :formatted-value "705-021-99-0008")
          (:value 3 :formatted-value "3"))))))))

(ert-deftest scoot-table--build-visual-model--result-without-metadata-still-creates-headers ()
  (let* ((result-model '((columns . ["Id" "RentalObjectCode"])
                         (rows . [])
                         (metadata (columns . []))))
         (table-model (scoot-table--build-visual-model result-model)))
    (should
     (equal
      (scoot--plist-select-keys table-model '(:headers :widths))
      '(:headers
        ((:name "Id" :header-label "Id" :metadata ((name . "Id")))
         (:name "RentalObjectCode" :header-label "RentalObjectCode" :metadata ((name . "RentalObjectCode"))))
        :widths (2 16))))))

;;; scoot-table.table-model.test.el ends here
