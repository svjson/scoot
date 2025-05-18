;;; scoot-xref-test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-xref)

(ert-deftest scoot-xref--destructure-xref--table ()
  (should
   (equal
    (scoot-xref--destructure-xref " i_am_the_table")
    (list :xref " i_am_the_table"
          :target :table
          :table "i_am_the_table"))))

(ert-deftest scoot-xref--destructure-xref--database ()
  (should
   (equal
    (scoot-xref--destructure-xref " big_can_of_data")
    (list :xref " big_can_of_data"
          :target :database
          :database "big_can_of_data"))))

(ert-deftest scoot-xref--destructure-xref--database/table ()
  (should
   (equal
    (scoot-xref--destructure-xref " big_can_of_data /  turned_table")
    (list :xref " big_can_of_data /  turned_table"
          :target :table
          :database "big_can_of_data"
          :table "turned_table"))))

(ert-deftest scoot-xref--destructure-xref--table/expr ()
  (should
   (equal
    (scoot-xref--destructure-xref " kangaroo_subspecies /  legs=2,heads=1")
    (list :xref " kangaroo_subspecies /  legs=2,heads=1"
          :target :expr
          :table "kangaroo_subspecies"
          :expr (list (list :lhs "legs"
                            :oper "="
                            :rhs "2")
                      (list :lhs "heads"
                            :oper "="
                            :rhs "1"))))))

;;; scoot-xref-test.el ends here
;; Local Variables:
;; flycheck-emacs-lisp-checkdoc-include-package-file: nil
;; End:
