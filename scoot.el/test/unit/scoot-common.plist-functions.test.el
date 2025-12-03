;;; scoot-common.plist-functions.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-common)
(require 'scoot-test-fixtures)


;; scoot--plist-get-in

(ert-deftest-parametrized scoot--plist-get-in
    (plist path expected)
    (("one-key-path--existing"
      (:literal (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:literal (:key-a))
      (:literal (:key-b "value-1" :key-c "value-2")))

     ("one-key-path--non-existing"
      (:literal (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:literal (:key-f))
      (:literal nil))

     ("two-key-path--existing"
      (:literal (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:literal (:key-a :key-c))
      (:literal "value-2"))

     ("five-key-path--existing"
      (:literal (:key-a (:key-b (:key-c (:key-d (:key-e "deeply stashed value"))))))
      (:literal (:key-a :key-b :key-c :key-d :key-e))
      (:literal "deeply stashed value"))

     ("five-key-path--partially-existing"
      (:literal (:key-a (:key-b (:key-c (:key-d (:key-e "deeply stashed value"))))))
      (:literal (:key-a :key-b :key-g :key-d :key-e))
      (:literal nil)))

  (should (equal (apply #'scoot--plist-get-in plist path)
                 expected)))


;; scoot--plist-select-keys

(ert-deftest scoot--plist-select-keys--cherry-picking-existing-keys ()
  (should (equal (scoot--plist-select-keys (list :headers '("A" "B" "C")
                                                 :tables '("Table1", "Table2")
                                                 :widths '(4 8 16))
                                           :headers :widths)
                 (list :headers '("A" "B" "C")
                       :widths '(4 8 16)))))




;;; scoot-common.plist-functions.test.el ends here
