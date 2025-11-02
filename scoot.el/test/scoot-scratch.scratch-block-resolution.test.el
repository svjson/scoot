;;; scoot-scratch.scratch-block-resolution.test.el --- summary -*- lexical-binding: t -*-

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

(require 'scoot-scratch)
(require 'ert)



(defmacro with-scratch-buffer (contents &rest body)
  "Run code in a temporary `scoot-scratch-mode' buffer.

Inserts CONTENTS into the temp buffer, activats scoot-scratch-mode
and executes BODY."
  `(with-temp-buffer
     (insert ,contents)
     (scoot-scratch-mode)
     ,@body))

(defun to-hash-table (plist)
  (let ((tbl (make-hash-table :test 'equal)))
    (while plist
      (puthash (pop plist) (pop plist) tbl))
    tbl))




(ert-deftest scoot-scratch--scratch-block-at-point--single-block--after-statement ()
  (with-scratch-buffer
   "
-- @context: client-A
-- @connection-name: app-production

SELECT * FROM users;

"
   (goto-char (point-max))
   (let ((block-props (cdr (scoot-scratch--scratch-block-at-point))))
     (should (equal (pp-to-string block-props)
                    (pp-to-string (to-hash-table (list "connection-name" "app-production"
                                                       "context" "client-A"))))))))


(ert-deftest scoot-scratch--scratch-block-at-point--multiple-blocks--in-first-block ()
  (with-scratch-buffer
   "
-- @context: client-A
-- @connection-name: app-production

SELECT * FROM users;


-- @context: client-B
-- @connection-name: local-dev

SELECT * FROM users;


"
   (goto-char (point-min))
   (forward-line 4)
   (let ((block-props (cdr (scoot-scratch--scratch-block-at-point))))
     (should (equal (pp-to-string block-props)
                    (pp-to-string (to-hash-table (list "connection-name" "app-production"
                                                       "context" "client-A"))))))))


(ert-deftest scoot-scratch--scratch-block-at-point--multiple-blocks--end-of-second-block ()
  (with-scratch-buffer
   "
-- @context: client-A
-- @connection-name: app-production

SELECT * FROM users;


-- @context: client-B
-- @connection-name: local-dev

SELECT * FROM users;


"
   (goto-char (point-max))
   (let ((block-props (cdr (scoot-scratch--scratch-block-at-point))))
     (should (equal (pp-to-string block-props)
                    (pp-to-string (to-hash-table (list "connection-name" "local-dev"
                                                       "context" "client-B"))))))))


(ert-deftest scoot-scratch--scratch-block-at-point--multiple-blocks-shared-contxt--end-of-second-block ()
  (with-scratch-buffer
   "
-- @context: client-A
-- @connection-name: app-production

SELECT * FROM users;


-- @connection-name: local-dev

SELECT * FROM users;


"
   (goto-char (point-max))
   (let ((block-props (cdr (scoot-scratch--scratch-block-at-point))))
     (should (equal (pp-to-string block-props)
                    (pp-to-string (to-hash-table (list "connection-name" "local-dev"
                                                       "context" "client-A"))))))))






;;; scoot-scratch.scratch-block-resolution.test.el ends here
