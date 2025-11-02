;;; scoot-connection.connection-management.test.el --- summary -*- lexical-binding: t -*-

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

(require 'scoot-connection)
(require 'scoot-test-fixtures)
(require 'ert)



(ert-deftest scoot-connection--store-connection--empty-context-store ()
  (clrhash scoot-contexts)
  (scoot-connection--store-connection "my-context" (list :name "my-conn"
                                                         :host "localhost"
                                                         :port "1412"))
  (should (equal (pp-to-string scoot-contexts)
                 (pp-to-string (scoot-test--to-hash-table
                                (list "my-context" (list "my-conn" (list :name "my-conn"
                                                                         :host "localhost"
                                                                         :port "1412"))))))))

(ert-deftest scoot-connection--store-connection--existing-context ()
  (clrhash scoot-contexts)
  (puthash "my-context" (list "existing-conn" (list :name "existing-conn"
                                                    :host "nonlocalhost"
                                                    :port "1431"))
           scoot-contexts)

  (scoot-connection--store-connection "my-context" (list :name "my-conn"
                                                         :host "localhost"
                                                         :port "1412"))
  (should (equal (pp-to-string scoot-contexts)
                 (pp-to-string (scoot-test--to-hash-table
                                (list "my-context" (list "existing-conn" (list :name "existing-conn"
                                                                               :host "nonlocalhost"
                                                                               :port "1431")
                                                         "my-conn" (list :name "my-conn"
                                                                         :host "localhost"
                                                                         :port "1412"))))))))




;;; scoot-connection.connection-management.test.el ends here
