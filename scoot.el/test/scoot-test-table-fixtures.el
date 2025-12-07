;;; scoot-test-table-fixtures.el --- test utilities/fixtures for scoot-table -*- lexical-binding: t -*-

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

;; This file contains fixtures and high-level functions for automating table
;; interaction during tests.

;;; Code:

(require 'scoot-test-fixtures)
(require 'scoot-input)
(require 'scoot-table)




(defun scoot-test-table--move-to-cell! (table cell)
  "Move point to CELL of TABLE."
  (interactively-goto-char (plist-get table :widget-start))
  (scoot-table--move-to-first-row!)
  (unless (fixnump (car cell))
    (error "Invalid cell identifier: %S" cell))
  (dotimes (_ (car cell))
    (do-command #'scoot-table--cell-down!))
  (unless (fixnump (cdr cell))
    (error "Invalid cell identifier: %S" cell))
  (dotimes (_ (cdr cell))
    (do-command #'scoot-table--cell-right!)))

(defun scoot-test-table--edit-cell! (table cell edit)
  "Edit CELL of TABLE according to EDIT."
  (scoot-test-table--move-to-cell! table cell)
  (do-command #'scoot-table--edit-cell)
  (dotimes (_ 6)
    (do-command #'delete-backward-char))
  (interactively-self-insert-text edit)
  (do-command #'scoot-input--confirm-edit))



(provide 'scoot-test-table-fixtures)


;;; scoot-test-table-fixtures.el ends here
