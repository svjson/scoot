;;; scoot-query-block.navigation.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-widget)
(require 'scoot-query-block)
(require 'scoot-test-fixtures)


;; enter and exit query block

(ert-deftest scoot-qb--query-block--enter-from-above--exit-bottom ()
  (with-new-window-buffer
    (insert "\n\n\n")
    (let ((widget (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere" (list :width 50))))
      (should (bound-and-true-p cursor-sensor-mode))
      ;; Move to start of buffer
      (call-interactively #'beginning-of-buffer)
      (run-hooks 'post-command-hook)
      (should (not (bound-and-true-p scoot-query-block-mode)))
      ;; Move three lines down - query block start
      (dotimes (_ 3)
        (call-interactively #'next-line)
        (run-hooks 'post-command-hook))
      (should (bound-and-true-p scoot-query-block-mode))
      ;; Move three lines down to last line of block
      (dotimes (_ 3)
        (call-interactively #'next-line)
        (run-hooks 'post-command-hook)
        (should (bound-and-true-p scoot-query-block-mode)))
      ;; Move one line down - out of block
      (call-interactively #'next-line)
      (run-hooks 'post-command-hook)
      (should (not (bound-and-true-p scoot-query-block-mode))))))




;;; scoot-query-block.navigation.test.el ends here
