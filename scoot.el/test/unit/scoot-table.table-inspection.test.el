;;; scoot-table.table-inspection.test.el --- summary -*- lexical-binding: t -*-

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
(require 'ert-parametrized)
(require 'scoot-table)
(require 'scoot-test-buffer-fixtures)
(require 'scoot-nexartrade-fixtures)
(require 'scoot-test-fixtures)



;; table-start

(ert-parametrized-deftest scoot-table--table-start
    (pre-content point-pos expected)
    (("table-at-bob--point-at-%s"
      (:eval "")
      (:generator (:quote '((1 . 0)
                            (1 . 8)
                            (2 . 0)
                            (2 . 10)
                            (5 . 0)
                            (5 . 5))))
      (:quote (1 . 1)))
     ("table-after-content--point-at-%s"
      (:eval "This is a message from our sponsor!\n\n")
      (:generator (:quote '((3 . 0)
                            (3 . 8)
                            (5 . 0)
                            (5 . 10)
                            (8 . 0)
                            (8 . 5))))
      (:quote (38 . 3))))
  (with-alphanum-keys
   (with-new-window-buffer
    (insert pre-content)
    (let ((table (scoot-table--insert-table! nexartrade-table--users--result-data)))
      (scoot-test-buffer--move-to-coord point-pos)

      (should (equal (scoot-table--table-start)
                     expected))
      (should (equal (scoot-table--table-start table)
                     expected))))))


;; cell-at-point

(ert-parametrized-deftest scoot-table--cell-at-point
    (point-pos expected)
    (("at-%s--is-cell-%s"
      (:generator (:quote '((4 . 3)
                            (4 . 18)
                            (5 . 5)
                            (5 . 18))))
      (:generator (:quote (list (list :row-index 0
                                      :column-index 0)
                                (list :row-index 0
                                      :column-index 1)
                                (list :row-index 1
                                      :column-index 0)
                                (list :row-index 1
                                      :column-index 1))))))
  (with-alphanum-keys
   (with-new-window-buffer
    (scoot-table--insert-table! nexartrade-table--users--result-data)

    (scoot-test-buffer--move-to-coord point-pos)


    (let ((cell (scoot-table--cell-at-point)))
      (should (equal (plist-get cell :row-index)
                     (plist-get expected :row-index)))

      (should (equal (plist-get cell :column-index)
                     (plist-get expected :column-index)))))))



;; cell-begin
(ert-parametrized-deftest scoot-table--cell-begin
    (point-pos expected)
    (("cell-at-%s-starts-at-%s"
       (:generator (:quote '((4 . 3)
                             (8 . 90)
                             (8 . 11))))
       (:generator (:quote '((558 . 4)
                             (1356 . 8)
                             (1306 . 8))))))
  (with-alphanum-keys
   (with-new-window-buffer
    (scoot-table--insert-table! nexartrade-table--users--result-data)

    (scoot-test-buffer--move-to-coord point-pos)

    (should (equal (scoot-table--cell-begin (point))
                   expected)))))



;; cell-end
(ert-parametrized-deftest scoot-table--cell-end
    (point-pos expected)
    (("cell-at-%s-starts-at-%s"
       (:generator (:quote '((4 . 3)
                             (8 . 90)
                             (8 . 11))))
       (:generator (:quote '((563 . 4)
                             (1420 . 8)
                             (1322 . 8))))))
  (with-alphanum-keys
   (with-new-window-buffer
    (scoot-table--insert-table! nexartrade-table--users--result-data)

    (scoot-test-buffer--move-to-coord point-pos)

    (should (equal (scoot-table--cell-end (point))
                   expected)))))





;;; scoot-table.table-inspection.test.el ends here
