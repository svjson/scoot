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
(require 'ert)

(defun scoot-test--with-buffer (contents point-pos fn)
  "Set up a temporary buffer with CONTENTS and call FN with point at POINT-POS."
  (with-temp-buffer
    (insert contents)
    (goto-char point-pos)
    (funcall fn)))

(ert-deftest scoot-statement-before-point--single-statement-without-semicolon ()
  (should (equal
             (let ((sql "SELECT * FROM orders"))
               (scoot-test--with-buffer sql
                                        (+ (length sql) 1)
                                        #'scoot-statement-before-point))
             "SELECT * FROM orders")))

(ert-deftest scoot-statement-before-point--single-statement-with-semicolon ()
    (should (equal
             (let ((sql "SELECT * FROM orders;"))
               (scoot-test--with-buffer sql
                                        (+ (length sql) 1)
                                        #'scoot-statement-before-point))
             "SELECT * FROM orders;")))

(ert-deftest scoot-statement-before-point--two-statements-same-line--run-from-end-of-line ()
    (should (equal
             (let ((sql "SELECT * FROM orders; SELECT * FROM customers;"))
               (scoot-test--with-buffer sql
                                        (+ (length sql) 1)
                                        #'scoot-statement-before-point))
             "SELECT * FROM customers;")))


(ert-deftest scoot--parse-context-annotation--connection-string-mssql ()
  (should (equal
           (scoot--parse-context-annotation
            "-- @connection-string: mssql+pyodbc://sa:secret@localhost:1433/tenants-leases?driver=ODBC+Driver+17+for+SQL+Server")
           '("connection-string" .
             "mssql+pyodbc://sa:secret@localhost:1433/tenants-leases?driver=ODBC+Driver+17+for+SQL+Server"))))

;;; scoot-test.el ends here


