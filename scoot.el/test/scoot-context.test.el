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

(require 'scoot-connection)
(require 'ert)



(ert-deftest scoot-context--connection-names--non-existing-context ()
  (clrhash scoot-contexts)
  (should (null (scoot-context--connection-names "hello-no-future"))))

(ert-deftest scoot-context--connection-names--only-existing-context ()
  (clrhash scoot-contexts)
  (puthash "my-website-project" (list :connections '(("test-db")
                                                     ("production")))
           scoot-contexts)
  (should (equal
           (scoot-context--connection-names "my-website-project")
           '("test-db" "production"))))

(ert-deftest scoot-context--connection-names--only-from-specified-context ()
  (clrhash scoot-contexts)
  (puthash "client-A" (list :connections '(("staging")
                                           ("local-dev")))
           scoot-contexts)
  (puthash "my-website-project" (list :connections '(("test-db")
                                                     ("production")))
           scoot-contexts)
  (puthash "client-B" (list :connections '(("dev-env")
                                           ("local-test")))
           scoot-contexts)
  (should (equal
           (scoot-context--connection-names "my-website-project")
           '("test-db" "production"))))

;;; scoot-context.test.el ends here
