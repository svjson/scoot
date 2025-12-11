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

(require 'cl-lib)
(require 'ert)
(require 'scoot-connection)
(require 'scoot-common)
(require 'scoot-table)

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

(defun scoot-test--use-connection (context-name connection-name)
  "Set the connection used for system tests using a registered context.

CONTEXT-NAME must exist in the scoot configuration and contain a
connection named CONNECTION-NAME."
  (let ((conn (scoot-context--get-connection context-name connection-name)))
    (unless conn
      (error "Unknown connection: %s / %s" context-name connection-name))
    (setq scoot-test--connection conn)
    (setq scoot-test--connection-name connection-name)))

(defun scoot-test--ensure-connection (callback)
  "Ensures that the `scoot-test--connection` is available.

Registers or confirms the configured connection, making sure that it
is available for use with a running `scoot-server`.

Invokes CALLBACK with the connection when done, if successful."
  (cond
   (scoot-test--connection-string
    (scoot-connection--register-connection callback scoot-test--connection))

   (scoot-test--connection
    (funcall callback scoot-test--connection))

   (t (error "No test connection configured"))))

(defun scoot-get-buffer-lines (from-point lines)
  "Get the buffer contents from FROM-POINT and n LINES onwards."
  (save-excursion
    (buffer-substring from-point (progn (goto-char from-point)
                                        (forward-line lines)
                                        (point)))))

(defun scoot-get-buffer-line-list (from-point lines &optional transform-fn)
  "Get the buffer contents from FROM-POINT and n LINES onwards in a list.

Optionally pass a transform-function TRANSFORM-FN to apply to each line."
  (let ((trans-fn (or transform-fn #'identity))
        (buffer-lines (string-split (scoot-get-buffer-lines from-point lines) "\n")))
    (mapcar
     trans-fn
     (cl-subseq buffer-lines 0 (min lines (length buffer-lines))))))

(defun scoot-get-result-header-lines ()
  "Get the result set header lines, including dividers, from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward "+--")
    (beginning-of-line)
    (scoot-get-buffer-line-list (point) 3 #'scoot-string-depropertize)))

(defun scoot-string-depropertize (str)
  "Remove all text properties from string STR."
  (set-text-properties 0 (length str) nil str)
  str)

(defun string-prefixes-p (string-list prefix-list)
  "Test if strings in STRING-LIST is prefixed by strings in PREFIX-LIST.

Return t if each string in TEST-LIST has a corresponding entry in REFERENCE-LIST
that it is a prefix of, otherwise nil."
  (when (equal (length string-list) (length prefix-list))
    (cl-every #'string-prefix-p prefix-list string-list)))



(defmacro with-request-backed-buffer (args mname body op)
  "Base macro for entering a buffer/mode backed by a server request.

ARGS contains symbol/value pairs that will be expanded into a let form.
These args may contain overrides for the default values of TIMEOUT=5 and
KEEP-BUFFER=nil.

MNAME is the name of the public macro.  Used only in error reporting.

BODY is the test body supplied to the top-level macro.

OP is the code that performs the server request and calls the buffer
setup code.  This code MUST set `test-buf' to the created buffer or a
timeout will be triggered, as the condition is that `test-buf' is no
longer nil.

When the test has finished execution - either by success, failure or
error - the buffer will be killed unless KEEP-BUFFER has been set to
a non-nil value."
  (declare (indent 3))
  `(let ((timeout 5)
         (keep-buffer nil))
     (let ,args
       (let ((test-buf nil))
         (unwind-protect
             (progn
               (scoot-test--ensure-connection
                (lambda (connection)
                  ,op))

               (with-timeout (timeout (error "Query timed out (timeout: %s)" timeout))
                 (while (null test-buf)
                   (sit-for 0.2)))

               (condition-case err
                   (with-current-buffer test-buf
                     ,@body)
                 (error
                  (message "Error during %s body execution: %S" ,mname err)
                  (signal (car err) (cdr err)))))

           (when (and (null keep-buffer) (buffer-live-p test-buf))
             (message "Killing the buffer now")
             (kill-buffer test-buf)))))))


(defmacro with-result-buffer (args &rest body)
  "Execute a query and open the result buffer with optional timeout.

ARGS is a bind form with the following possible keys:
  query       - The SQL query to execute.  Required.
  timeout     - The timeout in seconds.  Optional.
  keep-buffer - Keeps the resulting buffer if given a non-nil value.

BODY is the elisp code to execute once the query buffer has been opened."
  (declare (indent 1))
  `(with-request-backed-buffer
       ,args
       "with-result-buffer"
       ,body

     (scoot-connection--execute-statement
      connection query
      (lambda (result-context)
        (setq test-buf (scoot--open-resultset result-context
                                              :force-new-buffer t))))))

(put 'with-result-buffer 'lisp-indent-function 'defun)


(defmacro with-ddl-edit-mode-buffer (args &rest body)
  "Open the scoot-ddl-edit-mode with optional timeout.

ARGS is a bind form with the following possible keys:
  table       - The table schema to show.  Required.
  timeout     - The timeout in seconds.  Optional.
  keep-buffer - Keeps the resulting buffer if given a non-nil value.

BODY is the elisp code to execute once the query buffer has been opened."
  (declare (indent 1))
  `(with-request-backed-buffer
       ,args
       "with-ddl-edit-mode-buffer"
       ,body

     (scoot-connection--describe-table
      connection table
      (lambda (result-context)
        (setq test-buf (scoot-ddl--open-edit-mode result-context))))))


(defmacro with-ddl-mode-buffer (args &rest body)
  "Open the scoot-ddl-mode with optional timeout.

ARGS is a bind form with the following possible keys:
  table       - The table schema to show.  Required.
  timeout     - The timeout in seconds.  Optional.
  keep-buffer - Keeps the resulting buffer if given a non-nil value.

BODY is the elisp code to execute once the query buffer has been opened."
  (declare (indent 1) (debug ((&rest sexp) body)))
  `(with-request-backed-buffer
      ,args
      "with-ddl-mode-buffer"
      ,body

      (scoot-connection--describe-table
       connection table
       (lambda (result-context)
         (setq ddl-buf (scoot-ddl--open-table result-context))))))


(defun scoot-resultset-cell-summary-at (point)
  "Get a description representation of the resultset table cell at POINT."
  (save-excursion
    (let* ((cell-region (cons (1+ (previous-single-property-change (1+ point) 'thing))
                              (1- (next-single-property-change point 'thing)))))
      (let ((content (buffer-substring-no-properties (car cell-region) (cdr cell-region))))
        (goto-char (car cell-region))
        (while (and (<= (point) (cdr cell-region))
                    (equal " " (buffer-substring-no-properties (point) (1+ (point)))))
          (forward-char 1))
        (let ((props (scoot--props-at-point)))
          (list :column (alist-get 'name (alist-get 'column-meta props))
                :value (alist-get 'value props)
                :cell-content content
                :face (alist-get 'face props)))))))

(defun scoot-resultset-row-summary-at (point)
  "Get a description representation of every cell in the row at POINT."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (let ((next-cell-pos (car (scoot-table--next-cell (point)))))
      (if (null next-cell-pos)
          nil
        (progn
          (goto-char next-cell-pos)
          (let ((line (line-number-at-pos))
                (row nil))
            (while (eq line (line-number-at-pos))
              (setq row (push (scoot-resultset-cell-summary-at (point)) row))
              (setq next-cell-pos (car (scoot-table--next-cell (point))))
              (goto-char (or next-cell-pos (point-max))))
            (nreverse row)))))))

(defun scoot-resultset-first-line ()
  "Find the first result set row of a current result buffer.

Returns (<point> . <line-number>)."
  (save-excursion
    (goto-char (car (scoot-table--next-cell (point-min))))
    (when (scoot--thing-at-p (point) 'table-header)
      (forward-line 2))
    (beginning-of-line)
    (cons (point) (line-number-at-pos))))

(defun scoot-resultset-line (index)
  "Find table row INDEX in a result buffer.

Returns (<point> . <line-number>)."
  (save-excursion
    (goto-char (car (scoot-resultset-first-line)))
    (forward-line index)
    (cons (point) (line-number-at-pos))))

(defvar-local scoot-rs--result-data nil)

(defun scoot-resultset-row-identities (&optional start-row end-row)
  "Extract the identifying column values of the current result set.

A zero-arg invocation will return the identifying values of all rows
in the result set.

A subset can be selected using START-ROW and END-ROW.

If only START-ROW is provided, only that row index will be returned.

If END-ROW is also provided the values between START-ROW and END-ROW
\(inclusive) will be returned.

This is currently a naive implementation that simply assumes that
column with index 0 is the primary key.  This will obviously not do
once there are test for composite keys or uniqueness based relation
tables."
  (let* ((rows (alist-get 'rows scoot-rs--result-data))
         (end-row (if start-row
                      (1+ (or end-row start-row))
                    (if end-row (1+ end-row) (length rows))))
         (start-row (or start-row 0)))
    (mapcar
     (lambda (row) (aref row 0))
     (cl-subseq rows start-row end-row))))

(defun scoot-resultset-row-descriptions (start-row &optional end-row)
  "Get summary of cells between resultset rows START-ROW and END-ROW.

END-ROW is optional.  With only start-row supplied, this function returns a
single row."
  (let ((end-row (or end-row start-row)))
    (save-excursion
      (goto-char (car (scoot-resultset-line start-row)))
      (let ((result nil))
        (dotimes (_ (1+ (- end-row start-row)))
          (push (scoot-resultset-row-summary-at (point)) result)
          (forward-line 1))
        (nreverse result)))))

(defun list-elements-equal (actual expected)
  "Compare each list element separately between ACTUAL and EXPECTED."
  (cl-loop for act in actual
           for exp in expected
           for index from 0
           do (progn
                (unless (equal act exp)
                  (message (format "Mismatch at index %d:\n - Expected:\n%s\n - Actual:\n%s\n"
                                   index
                                   (pp-to-string exp)
                                   (pp-to-string act))))
                (should
                 (equal act exp))))
  t)

(defun row-description-equal (actual expected)
  "Compare a result set row description between ACTUAL and EXPECTED.

Delegates to list-elements-equal for deep comparison/reporting."
  (cl-loop for act in actual
           for exp in expected
           for index from 0
           do (progn
                (unless (equal act exp)
                  (message (format "Mismatch at element %d:\n - Expected:\n%s\n - Actual:\n%s\n"
                                   index
                                   (pp-to-string exp)
                                   (pp-to-string act))))
                (should (list-elements-equal act exp))))
  t)


(defun row-descriptions-equal (actual expected)
  "Compare result set descriptions ACTUAL and EXPECTED.

Delegates to row-description-equal for comparing each row separately to simplify
ocular inspection of diffs."
  (cl-loop for act in actual
           for exp in expected
           for index from 0
           do (progn
                (unless (equal act exp)
                  (message (format "Mismatch at row %d:\n - Expected:\n%s\n - Actual:\n%s\n"
                                   index
                                   (pp-to-string exp)
                                   (pp-to-string act))))
                (row-description-equal act exp)))
  t)

(provide 'scoot-test-base)
;;; scoot-test-base.el ends here
