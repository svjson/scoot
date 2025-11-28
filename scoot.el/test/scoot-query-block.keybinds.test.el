;;; scoot-query-block.keybinds.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-resultset)
(require 'scoot-connection)
(require 'scoot-test-fixtures)
(require 'scoot-nexartrade-fixtures)

(ert-deftest scoot-query-block--keybinding-overlays--q->quit-window--or--self-insert-command ()
  (with-fake-connection "nexartrade" (nexartrade-fake-connection)
    (let ((ddl-buffer (scoot--open-resultset (nexartrade-result-context--table-users))))
      (with-current-buffer ddl-buffer
        (let ((query-block (scoot-widget--get-widget :type 'query-block
                                                     :name 'query-block)))
          (interactively-goto-line 1)
          (should (equal (key-binding (kbd "q")) #'quit-window))
          (interactively-goto-line (1- (plist-get query-block :widget-start-line)))
          (should (equal (key-binding (kbd "q")) #'quit-window))
          (interactively-goto-line (plist-get query-block :widget-start-line))
          ;; (should (bound-and-true-p scoot-query-block-mode))
          ;; (should (equal (key-binding (kbd "q")) #'self-insert-command))
          ;; (kill-buffer ddl-buffer)
          )))))


;;; scoot-query-block.keybinds.test.el ends here
