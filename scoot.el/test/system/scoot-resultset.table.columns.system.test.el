;;; scoot-resultset.table.columns.system.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot)
(require 'scoot-test-base)

(ert-deftest result--verify-column-headers-with-primary-and-foreign-keys ()
  (with-alphanum-keys
   (with-result-buffer ((query "SELECT * FROM nexartrade_staging.orders")
                        (timeout 5))
     (should
      (string-prefixes-p
       (scoot-get-result-header-lines)
       (list
        "+-------+------------+--------------+"
        "| PK id | FK user_id | order_number |"
        "+-------+------------+--------------+"))))))

(ert-deftest result--verify-cell-rendering--nexartrade-products ()
  (with-alphanum-keys
   (with-result-buffer ((query "SELECT * FROM nexartrade_staging.products WHERE id=1")
                        (keep-buffer t))
     (should
      (row-descriptions-equal
       (scoot-resultset-row-descriptions 0)
       (list
        (list
         (list :column "id"
               :value 1
               :cell-content "    1"
               :face 'scoot-cell-number-face)
         (list :column "name"
               :value "Automated neutral Graphical User Interface"
               :cell-content "Automated neutral Graphical User Interface"
               :face 'scoot-cell-string-face)
         (list :column "description"
               :value "Ground create camera nearly necessary woman herself job."
               :cell-content "Ground create camera nearly necessary woman herself job."
               :face 'scoot-cell-string-face)
         (list :column "sku"
               :value "SKU0001"
               :cell-content "SKU0001"
               :face 'scoot-cell-string-face)
         (list :column "price"
               :value 391.09
               :cell-content "391.09"
               :face 'scoot-cell-number-face)
         (list :column "in_stock"
               :value 52
               :cell-content "      52"
               :face 'scoot-cell-number-face)
         (list :column "created_at"
               :value "2023-03-05T05:30:53"
               :cell-content "2023-03-05T05:30:53"
               :face 'scoot-cell-temporal-face))))))))

;;; scoot-resultset.table.columns.system.test.el ends here
