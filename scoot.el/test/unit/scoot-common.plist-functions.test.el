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
(require 'ert-parametrized)
(require 'scoot-common)
(require 'scoot-test-fixtures)


;; scoot--plist-get-in

(ert-parametrized-deftest scoot--plist-get-in
    (plist path expected)
    (("one-key-path--existing"
      (:quote (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:quote (:key-a))
      (:quote (:key-b "value-1" :key-c "value-2")))

     ("one-key-path--non-existing"
      (:quote (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:quote (:key-f))
      (:eval nil))

     ("two-key-path--existing"
      (:quote (:key-a (:key-b "value-1" :key-c "value-2") (:key-d "value-3")))
      (:quote (:key-a :key-c))
      (:eval "value-2"))

     ("five-key-path--existing"
      (:quote (:key-a (:key-b (:key-c (:key-d (:key-e "deeply stashed value"))))))
      (:quote (:key-a :key-b :key-c :key-d :key-e))
      (:eval "deeply stashed value"))

     ("five-key-path--partially-existing"
      (:quote (:key-a (:key-b (:key-c (:key-d (:key-e "deeply stashed value"))))))
      (:quote (:key-a :key-b :key-g :key-d :key-e))
      (:eval nil)))

  (should (equal (apply #'scoot--plist-get-in plist path)
                 expected)))


;; scoot--plist-put-in

(ert-parametrized-deftest scoot--plist-put-in
    (plist path value expected)
    (("put-new-key-into-nested"
      (:quote (:key-a (:key-b "value-1")))
      (:quote (:key-a :key-c))
      (:eval "value-2")
      (:quote (:key-a (:key-b "value-1" :key-c "value-2"))))
     ("put-new-key-at-root"
      (:quote (:key-a "value-1"))
      (:quote (:key-b))
      (:eval "value-2")
      (:quote (:key-a "value-1" :key-b "value-2"))))

  ;; When
  (scoot--plist-put-in plist path value)

  ;; Then
  (should (equal plist expected)))


;; scoot--plist-select-keys

(ert-deftest scoot--plist-select-keys--cherry-picking-existing-keys ()
  (should (equal (scoot--plist-select-keys (list :headers '("A" "B" "C")
                                                 :tables '("Table1", "Table2")
                                                 :widths '(4 8 16))
                                           :headers :widths)
                 (list :headers '("A" "B" "C")
                       :widths '(4 8 16)))))




;;; scoot-common.plist-functions.test.el ends here
