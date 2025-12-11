;;; scoot-test-customization-fixtures.el --- fixtures for temporary setting custom variables -*- lexical-binding: t -*-

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

;; This file contains fixtures and macros for temporarily manipulating
;; custom variables

;;; Code:

(require 'scoot-test-fixtures)
(require 'scoot-input)
(require 'scoot-table)




(defmacro with-customized-variables (settings &rest body)
  "Temporarily set custom variables and restore them afterwards.

SETTINGS should have the form: ((some-setting-name value)
                                (another-setting another-value))
BODY is the elisp code to execute while SETTINGS are active."
  (declare (indent 1))
  `(let ((orig-values (mapcar (lambda (s) (cons s (symbol-value s))) ',(mapcar #'car settings))))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (s) `(customize-set-value ',(car s) ,(cadr s))) settings)
           ,@body)
       (dolist (pair orig-values)
         (customize-set-value (car pair) (cdr pair))))))


(defmacro with-alphanum-keys (&rest body)
  "Set alphanumeric primary and foreign keys icons and restore them afterwards.

BODY is the elisp code to execute while the customized key icons are active."
  `(with-customized-variables
     ((scoot-primary-key-icon "PK ")
      (scoot-foreign-key-icon "FK "))
     ,@body))



(provide 'scoot-test-customization-fixtures)


;;; scoot-test-customization-fixtures.el ends here
