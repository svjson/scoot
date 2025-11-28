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

(require 'cl-lib)


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



;; Keyboard command simulation

(defun do-command (cmd)
  "Call CMD interactively and run command hooks."
  (let ((this-command cmd))
    (run-hooks 'pre-command-hook)
    (call-interactively cmd)
    (run-hooks 'post-command-hook)))

(defun interactively-goto-char (pt)
  "Go to point PT, interactively and triggering hooks."
  (goto-char (point-min))
  (let ((target-line (line-number-at-pos pt)))
    (while (not (= (line-number-at-pos) target-line))
      (do-command #'next-line))
    (while (not (= (point) pt))
      (do-command #'forward-char))))

(defun interactively-self-insert-char (ch)
  "Simulate `self-insert-command`, inserting CH."
  (let ((last-command-event ch))
    (do-command #'self-insert-command)))



;; Should equal with custom fail message

(defmacro should-equal (expected actual &optional msg)
  "Equal assertion allowing a custom fail message MSG.

Calls `ert-fail` unless EXPECTED passes `equal` check with
ACTUAL."
  `(let ((e ,expected)
         (a ,actual))
     (unless (equal e a)
       (ert-fail
        (format "%sExpected: %S   Actual: %S"
                (or ,msg "")
                e a)))))



;; Content insertion

(defun insert-whitespace-block (columns lines)
  "Insert a COLUMNS wide whitespace block of LINES height."
  (dotimes (_ lines)
    (insert (make-string columns ?\s))
    (insert "\n")))


;; Data structures

(defun scoot-test--normalize-struct (struct)
  "Normalize data structure STRUCT for asserts.

Used to replace bytecode objects and function references to
something more handy for comparison of results with
inline data structures using `equals`."
  (cond
   ;; Remove byte-code objects
   ((or (interpreted-function-p struct)
        (byte-code-function-p struct))
    (format "%s" struct))

   ;; Recurse alists/plists
   ((and (consp struct) (symbolp (car struct)))
    (cons (car struct) (scoot-test--normalize-struct (cdr struct))))

   ;; Recurse lists
   ((listp struct) (mapcar #'scoot-test--normalize-struct struct))

   ;; Vectors
   ((vectorp struct) (cl-map 'vector #'scoot-test--normalize-struct struct))

   (t struct)))

(defun scoot-test--to-hash-table (plist)
  "Convert PLIST to hash table."
  (let ((tbl (make-hash-table :test 'equal)))
    (while plist
      (puthash (pop plist) (pop plist) tbl))
    tbl))



(provide 'scoot-test-fixtures)

;;; scoot-test-fixtures.el ends here
