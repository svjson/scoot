;;; scoot-ddl.edit.query-block.system.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-widget)
(require 'scoot-ddl)
(require 'scoot-test-base)
(require 'scoot-test-fixtures)



(ert-deftest ddl-edit--existing-table--query-block-is-present ()
  (with-alphanum-keys
   (with-ddl-mode ((table "users")
                   (timeout 5)
                   (edit-mode t))
     (let ((query-block (scoot-widget--get-widget :type 'query-block
                                                  :name 'query-block)))
       (should query-block)
       (let ((ddl-content (scoot-widget--shadow-buffer-content query-block)))
         (should (string-match-p "CREATE TABLE" ddl-content))
         (should (string-match-p "users" ddl-content))
         (should (string-match-p "id" ddl-content))
         (should (string-match-p "email" ddl-content))
         (should (string-match-p "password_hash" ddl-content))
         (should (string-match-p "created_at" ddl-content))
         (should (string-match-p "last_login" ddl-content))
         (should (string-match-p "is_active" ddl-content)))))))

(ert-deftest ddl-edit--existing-table--query-block-is-editable ()
  (with-alphanum-keys
   (with-ddl-mode ((table "users")
                   (timeout 5)
                   (edit-mode t))
     (let* ((query-block (scoot-widget--get-widget :type 'query-block
                                                   :name 'query-block))
            (original-content (scoot-widget--shadow-buffer-content query-block)))
       (interactively-goto-char (plist-get query-block :widget-start))
       (should (bound-and-true-p scoot-query-block-mode))
       (interactively-self-insert-char ?a)
       (should (equal (scoot-widget--shadow-buffer-content query-block)
                      (concat "a" original-content)))))))



;;; scoot-ddl.edit.query-block.system.test.el ends here
