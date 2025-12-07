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



;; Buffer management

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


(defmacro with-alphanum-keys (&rest body)
  "Set alphanumeric primary and foreign keys icons and restore them afterwards.

BODY is the elisp code to execute while the customized key icons are active."
  `(with-customized-variables
     ((scoot-primary-key-icon "PK ")
      (scoot-foreign-key-icon "FK "))
     ,@body))



;; Buffer state & debug

(defun scoot-active-minor-modes ()
  "Round up active minor modes relevant to scoot and scoot tests."
  (cl-remove-if-not
   (lambda (mode)
	   (memq mode '(cursor-sensor-mode
                  read-only-mode
                  scoot-query-block-mode
                  scoot-input-mode
                  scoot-table-mode)))
   local-minor-modes))

(defun scoot-command-hooks ()
  "Get the active scoot-related command hooks."
  (cl-flet ((filter-hooks (hook-list white-list)
              (mapcar
               (lambda (h)
                 (let ((hstr (format "%S" h)))
                   (if (interpreted-function-p h)
                       (let* ((spos (string-search "scoot" hstr)))
                         (intern (substring hstr spos (string-search " " hstr spos))))
                     h)))
               (seq-filter (lambda (h)
                             (or (memq h white-list)
                                 (string-search "scoot" (format "%S" h))))
                           hook-list))))
    (list :pre-command-hook (filter-hooks pre-command-hook '())
          :post-command-hook (filter-hooks post-command-hook '(cursor-sensor--detect)))))

(defun scoot-buffer-widgets ()
  "Round up widgets present in the current buffer."
  (cl-flet ((widget-edge (w prop)
              (let ((pos (plist-get (cdr w) prop)))
                (list :point (marker-position  pos)
                      :pos (when pos (cons (line-number-at-pos pos)
                                           (save-excursion
                                             (goto-char pos)
                                             (current-column))))))))
    (mapcar (lambda (w)
              (list :id (car w)
                    :from (widget-edge w :widget-start)
                    :to (widget-edge w :widget-end)))
            scoot--active-widgets)))

(defun scoot-buffer-state ()
  "Round up relevant scoot buffer state."
  (append
   (list :major-mode major-mode
         :minor-modes (scoot-active-minor-modes)
         :widgets (scoot-buffer-widgets))
   (scoot-command-hooks)))



;; Keyboard command simulation

(defun do-command (cmd)
  "Call CMD interactively and run command hooks."
  (let ((this-command cmd))
    (run-hooks 'pre-command-hook)
    (call-interactively cmd)
    (run-hooks 'post-command-hook)))

(defun interactively-goto-char (pt)
  "Go to point PT, interactively and triggering hooks."
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
