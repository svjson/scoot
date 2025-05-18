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
(require 'scoot-common)

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


(defmacro with-customized-variables (settings &rest body)
  "Temporarily set custom variables and restore them afterwards.

SETTINGS should have the form: ((some-setting-name value)
                                (another-setting another-value))
BODY is the elisp code to execute while SETTINGS are active."
  `(let ((orig-values (mapcar (lambda (s) (cons s (symbol-value s))) ',(mapcar #'car settings))))
     (unwind-protect
         (progn
           ,@(mapcar (lambda (s) `(customize-set-value ',(car s) ,(cadr s))) settings)
           ,@body)
       (dolist (pair orig-values)
         (customize-set-value (car pair) (cdr pair))))))

(defmacro with-alphanum-keys (&rest body)
  "Set alphanumeric primary and foreign keys icons and restore them afterwards.

BODY is the elisp code to execute while the customized key icons are active."
  `(with-customized-variables
       ((scoot-primary-key-icon "PK ")
        (scoot-foreign-key-icon "FK "))
     ,@body))

(put 'with-customized-variables 'lisp-indent-function 'defun)

(defmacro with-result-buffer (args &rest body)
  "Execute a query and open the result buffer with optional timeout.

ARGS is bind form with the following possible keys:
  query       - The SQL query to execute.  Required.
  timeout     - The timeout in seconds.  Optional.
  keep-buffer - Keeps the resulting buffer if given a non-nil value.

BODY is the elisp code to execute once the query buffer has been opened."
  (declare (indent 1) (debug ((&rest sexp) body)))
  `(let ((timeout 5)
         (keep-buffer nil))
     (let ,args
       (let ((result-buf nil))
         (unwind-protect
             (progn
               (scoot-connection--register-connection
                (lambda (connection)
                  (scoot-connection--execute-statement
                   connection query
                   (lambda (result-context)
                     (setq result-buf (scoot-result--open-result-buffer result-context)))))

                scoot-test--connection-name
                scoot-test--connection-string)

               (with-timeout (timeout (error "Query timed out (timeout: %s)" timeout))
                 (while (null result-buf)
                   (sit-for 0.2)))

               (with-current-buffer result-buf
                 ,@body))

           (when (and (null keep-buffer) (buffer-live-p result-buf))
             (kill-buffer result-buf)))))))

(put 'with-result-buffer 'lisp-indent-function 'defun)

(defun scoot-resultset-next-cell (&optional point)
  "Find the location of the the next table-cell of the buffer.

If POINT is not provided, the search will start from the beginning
of the buffer."
  (save-excursion
    (goto-char (or point (point-min)))
    (let ((point (next-single-property-change (point) 'thing)))
      (while (and (not (null point))
                  (not (equal 'table-cell (cdr (assoc 'thing (scoot--props-at point))))))
        (setq point (next-single-property-change point 'thing)))
      (if (null point)
          nil
        (cons point (line-number-at-pos))))))

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
        (let ((props (scoot--props-at (point))))
          (list :column (alist-get 'name (alist-get 'column-meta props))
                :value (alist-get 'value props)
                :cell-content content
                :face (alist-get 'face props)))))))

(defun scoot-resultset-row-summary-at (point)
  "Get a description representation of every cell in the row at POINT."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (let ((next-cell-pos (car (scoot-resultset-next-cell (point)))))
      (if (null next-cell-pos)
          nil
        (progn
          (goto-char next-cell-pos)
          (let ((line (line-number-at-pos))
                (row nil))
            (while (eq line (line-number-at-pos))
              (setq row (push (scoot-resultset-cell-summary-at (point)) row))
              (setq next-cell-pos (car (scoot-resultset-next-cell (point))))
              (goto-char (or next-cell-pos (point-max))))
            (nreverse row)))))))

(defun scoot-resultset-first-line ()
  "Find the first result set row of a current result buffer.

Returns (<point> . <line-number>)."
  (save-excursion
    (goto-char (car (scoot-resultset-next-cell (point-min))))
    (beginning-of-line)
    (cons (point) (line-number-at-pos))))

(defun scoot-resultset-line (index)
  "Find table row INDEX in a result buffer.

Returns (<point> . <line-number>)."
  (save-excursion
    (goto-char (car (scoot-resultset-first-line)))
    (forward-line index)
    (cons (point) (line-number-at-pos))))

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
