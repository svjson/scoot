;;; scoot-query-block.shadow-buffer-sync.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-test-buffer-fixtures)


;; Walk query block with "arrow keys" commands

(ert-deftest scoot-qb--query-block--shadow-buffer-pos--walk-down-col-0 ()
  (with-new-window-buffer
   (insert "\n\n\n")
   (let ((widget (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere" (list :width 50))))
     (insert "\n\n")
     ;; Go to beginning of line above query block
     (goto-char 3)
     ;; Enter query block
     (do-command #'next-line)
     (should (equal (scoot-widget--shadow-buffer-point widget) 1))
     (should (equal (point) 4))
     ;; Go to the second query block line
     (do-command #'next-line)
     (should (equal (scoot-widget--shadow-buffer-point widget) 8))
     (should (equal (point) 54))
     ;; Go to the third query block line
     (do-command #'next-line)
     (should (equal (scoot-widget--shadow-buffer-point widget) 12))
     (should (equal (point) 104))
     ;; Go to the fourth query block line
     (do-command #'next-line)
     (should (equal (scoot-widget--shadow-buffer-point widget) 17))
     (should (equal (point) 154))
     ;; Exit the query block
     (do-command #'next-line)
     (should (equal (scoot-widget--shadow-buffer-point widget) 17))
     (should (equal (point) 204)))))


(ert-deftest scoot-qb--query-block--shadow-buffer-pos--walk-up-col-0 ()
  (with-new-window-buffer
   (insert "\n\n\n")
   (let ((widget (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere" (list :width 50))))
     (insert "\n\n")
     ;; Go to beginning of line below query block
     (goto-char (plist-get widget :widget-end))
     (should (equal 204 (point)))
     ;; Enter query block
     (do-command #'previous-line)
     (should (equal 17 (scoot-widget--shadow-buffer-point widget)))
     (should (equal 154 (point)))
     ;; Go to the fourth query block line
     (do-command #'previous-line)
     (should (equal 12 (scoot-widget--shadow-buffer-point widget)))
     (should (equal 104 (point)))
     ;; Go to the third query block line
     (do-command #'previous-line)
     (should (equal 8 (scoot-widget--shadow-buffer-point widget)))
     (should (equal 54 (point)))
     ;; Go to the second query block line
     (do-command #'previous-line)
     (should (equal 1 (scoot-widget--shadow-buffer-point widget)))
     (should (equal 4 (point)))
     ;; Exit the query block
     (do-command #'previous-line)
     (should (equal 1 (scoot-widget--shadow-buffer-point widget)))
     (should (equal 3 (point))))))


(ert-deftest scoot-qb--query-block--shadow-buffer-pos--walk-right-start-to-end ()
  (with-new-window-buffer
   (insert "\n\n\n")
   (let ((widget (scoot-qb--insert-query-block! "SELECT\n  *\nFROM\n  somewhere" (list :width 50))))
     (insert "\n\n")
     ;; Go to beginning of line below query block
     (goto-char 3)
     ;; Enter query block
     (do-command #'right-char)
     (should (equal 1 (scoot-widget--shadow-buffer-point widget)))
     ;; Walk right across the first line
     (dotimes (i 6)
       (do-command #'right-char)
       (should-equal (+ 2 i) (scoot-widget--shadow-buffer-point widget)
                     (format "Line #1 - Shadow buffer position mismatch at iteration %s" i))
       (should-equal (+ 5 i) (point)
                     (format "Line #1 - Visible buffer position mismatch at iteration %s" i)))
     (should (equal (line-number-at-pos) 4))

     ;; Walk right across the second line
     (dotimes (i 4)
       (do-command #'right-char)
       (should-equal (+ 8 i) (scoot-widget--shadow-buffer-point widget)
                     (format "Line #2 - Shadow buffer position mismatch at iteration %s" i))
       (should-equal (+ 54 i) (point)
                     (format "Line #2 - Visible buffer position mismatch at iteration %s" i)))
     (should (equal (line-number-at-pos) 5))

     ;; Walk right across the third line
     (dotimes (i 5)
       (do-command #'right-char)
       (should-equal (+ 12 i) (scoot-widget--shadow-buffer-point widget)
                     (format "Line #3 - Shadow buffer position mismatch at iteration %s" i))
       (should-equal (+ 104 i) (point)
                     (format "Line #3 - Visible buffer position mismatch at iteration %s" i)))
     (should (equal (line-number-at-pos) 6))

     ;; Walk right across the fourth line
     (dotimes (i 11)
       (do-command #'right-char)
       (should-equal (+ 17 i) (scoot-widget--shadow-buffer-point widget)
                     (format "Line #4 - Shadow buffer position mismatch at iteration %s" i))
       (should-equal (+ 154 i) (point)
                     (format "Line #4 - Visible buffer position mismatch at iteration %s" i)))

     (do-command #'right-char)
     (should (equal (line-number-at-pos) 7)))))




;;; scoot-query-block.shadow-buffer-sync.test.el ends here
