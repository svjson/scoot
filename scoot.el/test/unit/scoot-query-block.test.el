;;; scoot-query-block.test.el --- summary -*- lexical-binding: t -*-

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



;; scoot-qb--insert-query-block!

(ert-deftest scoot-qb--insert-query-block!--single-line-at-row-4 ()
  (with-temp-buffer
    (insert "\n\n\n")
    (let ((widget (scoot-qb--insert-query-block! "SELECT * FROM somewhere" (list :width 50))))
      (should (equal 4 (marker-position (plist-get widget :widget-start))))
      (should (equal 4 (marker-position (plist-get widget :editable-start))))
      (should (equal 54 (marker-position (plist-get widget :widget-end))))
      (should (equal 54 (marker-position (plist-get widget :editable-end))))
      (should (equal 54 (point))))))

(ert-deftest scoot-qb--insert-query-block!--multiline-at-row-8 ()
  (with-temp-buffer
    (insert "\nHere's some text\n\n\n\n\n\n\n")
    (let ((widget (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere\n" (list :width 25))))
      (should (equal 25 (marker-position (plist-get widget :widget-start))))
      (should (equal 25 (marker-position (plist-get widget :editable-start))))
      (should (equal 150 (marker-position (plist-get widget :widget-end))))
      (should (equal 150 (marker-position (plist-get widget :editable-end))))
      (should (equal 150 (point)))

      ;; Validate line positions
      (goto-char 25)
      (should (equal 0 (current-column)))
      (should (equal 9 (line-number-at-pos)))
      (forward-line 1)
      (should (equal 0 (current-column)))
      (should (equal 10 (line-number-at-pos))))))

(ert-deftest scoot-qb--insert-query-block!--activates-cursor-sensor-mode ()
  (with-temp-buffer
    (should (not (bound-and-true-p cursor-sensor-mode)))
    (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere\n" (list :width 25))
    (should (bound-and-true-p cursor-sensor-mode))))



;; scoot-qb--get-query

(ert-deftest scoot-qb--get-query--unedited ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block! "SELECT * FROM somewhere")))
      (should (equal (scoot-qb--get-query :widget widget)
                     "SELECT * FROM somewhere")))))



;; scoot-qb--query-block-at-p

(ert-deftest scoot-qb--query-block-at-p ()
  (with-temp-buffer
    (insert "\n\n")
    (scoot-qb--insert-query-block! "SELECT * FROM\n somewhere")
    (insert "\n\n")
    (should (equal nil (scoot-qb--query-block-at-p 1)))
    (should (equal nil (scoot-qb--query-block-at-p 2)))
    (should (equal t (scoot-qb--query-block-at-p 3)))
    (goto-char 3)
    (forward-line 1)
    (should (equal t (scoot-qb--query-block-at-p (point))))
    (forward-line 1)
    (should (equal nil (scoot-qb--query-block-at-p (point))))))





;;; scoot-query-block.test.el ends here
