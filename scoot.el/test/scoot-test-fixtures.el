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

(require 'scoot-widget)



;; Async

(defmacro wait-until (condition &optional timeout)
  "Block until CONDITION evaluates to non-nil.

Optionally override the default TIMEOUT value of 3 seconds."
  (let ((timeout (or timeout 3)))
    `(with-timeout (,timeout (error "Timeout waiting for condition %S" (quote ,condition)))
       (while (not ,condition)
         (sit-for 0.2)))))



;; Keyboard command simulation

(defun do-command (cmd)
  "Call CMD interactively and run command hooks."
  (let ((this-command cmd)
        (post-command-hook post-command-hook))
    (run-hooks 'pre-command-hook)
    (call-interactively cmd)
    (run-hooks 'post-command-hook)))

(defun interactively-goto-char (pt)
  "Go to point PT, interactively and triggering hooks."
  (unless (number-or-marker-p pt)
    (error "interactively-goto-char: Invalid argument: %S" pt))
  (interactively-goto-line (line-number-at-pos pt))
  (while (not (= (point) pt))
    (do-command (if (< (point) pt)
                    #'forward-char
                  #'backward-char))))

(defun interactively-goto-line (line-num)
  "Go to line LINE-NUM, interactively and triggering hooks."
  (goto-char (point-min))
  (while (not (= (line-number-at-pos) line-num))
    (message "%s, %s" (line-number-at-pos) (current-column))
    (do-command (if (< (line-number-at-pos) line-num)
                    #'next-line
                  #'previous-line))))

(defun interactively-self-insert-char (ch)
  "Simulate `self-insert-command`, inserting CH."
  (let ((last-command-event ch))
    (do-command #'self-insert-command)))

(defun interactively-self-insert-text (text)
  "Simulate typing TEXT, char by char."
  (mapc #'interactively-self-insert-char text))



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



;; State fixtures

(defmacro with-fake-connection (context-name connection &rest body)
  "Execute BODY with CONTEXT-NAME containing CONNECTION registered."
  (declare (indent 1))
  `(let ((scoot-contexts (make-hash-table :test #'equal)))
     (puthash ,context-name
              (list :connections (list (cons (plist-get ,connection :name) ,connection)))
              scoot-contexts)
     ,@body))
(put 'with-fake-connection 'lisp-indent-function 'defun)



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
