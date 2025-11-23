;;; scoot-test-fixtures.el --- summary -*- lexical-binding: t -*-

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



;; Window buffer management

(defmacro with-new-window-buffer (&rest body)
  "Macro that executes BODY within a \"real\" temporary buffer.

Unlike `with-temp-buffer` this buffer attaches to the window, allowing
commands and hooks to run properly."
  `(let ((buf (generate-new-buffer "*scoot test-buffer*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          ,@body)
        (kill-buffer buf))))



(defun scoot-test--to-hash-table (plist)
  (let ((tbl (make-hash-table :test 'equal)))
    (while plist
      (puthash (pop plist) (pop plist) tbl))
    tbl))



(provide 'scoot-test-fixtures)

;;; scoot-test-fixtures.el ends here
