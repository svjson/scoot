;;; scoot-test-base.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2025 Sven Johansson

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

;; Contains features and functions commonly used and re-used by scoot ert
;; test suites.

;;; Code:

(defvar scoot-test--connection nil
  "Connection object to use for integration tests.  Set by runner.")
(defvar scoot-test--connection-name nil
  "Connection name to use for integration tests.  Set by runner.")
(defvar scoot-test--connection-string nil
  "Connection url to use for integration tests.  Set by runner.")

(defun scoot-test--set-connection (conn)
  "Set the connection details used by integration tests to CONN."
  (setq scoot-test--connection conn)
  (setq scoot-test--connection-name (plist-get conn :name))
  (setq scoot-test--connection-string (plist-get conn :url)))

(defun scoot-get-buffer-lines (from-point lines)
  "Get the buffer contents from FROM-POINT and n LINES onwards."
  (save-excursion
    (buffer-substring from-point (progn (goto-char from-point)
                                        (forward-line lines)
                                        (point)))))

(defun scoot-string-depropertize (str)
  "Remove all text properties from string STR."
  (set-text-properties 0 (length str) nil str)
  str)

(provide 'scoot-test-base)
;;; scoot-test-base.el ends here
