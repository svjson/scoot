;;; scoot-dsl.el --- summary -*- lexical-binding: t -*-

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

;; Contains an ergonomic convenience syntax for interacting with databases
;; via scoot.el and the scoot database.

;;; Code:

(require 'cl-lib)
(require 'scoot-connection)
(require 'scoot-resultset)



(defvar-local scootq--current-connection nil)

(defmacro with-scoot-connection (connection &rest body)
  "Macro for executing BODY with CONNECTION as the current connection.

The connection will be available within BODY as `scootq--current-connection`."
  (declare (indent 1))
  `(let ((scootq--current-connection ',connection))
     (message "%s" scootq--current-connection)
     (message "%s" ',connection)
     ,@body))

(defun scootq--query-sync (connection query)
  "Synchronous wrapper for async queries, using using CONNECTION to perform QUERY.

Uses the async scoot-connect--execute-statement function to perform
the query and blocks until success or error."
  (message "Using: %s" connection)
  (let ((done nil)
        (result nil)
        (error nil))

    (apply #'scoot-connection--execute-statement
           (append (list connection query)
                   (list (lambda (data)
                           (setq result data)
                           (setq done t)))))

    (while (not done)
      (accept-process-output nil 0.05))

    result))

(defun scootq-column-index (result-set column-name)
  "Return the index of COLUMN-NAME in RESULT-SET."
  (cl-position column-name
               (alist-get 'columns result-set)
               :test #'equal))

(defun scootq-column (column-name result-data)
  "Return the values of COLUMN-NAME from RESULT-DATA."
  (let ((result-set (plist-get result-data :result)))
    (mapcar (lambda (record)
              (aref record
                    (scootq-column-index result-set column-name)))

            (alist-get 'rows result-set))))

(defun scootq-empty-result-p (result-data)
  "Return t if RESULT-DATA contains no rows."
  (= 0 (length  (alist-get 'rows (plist-get result-data :result)))))

(cl-defun scootq-group (&key by keep data)
  "Group the records in DATA by the column specified in BY.

If KEEP is specified with a :column key, only that column's value will be kept."
  (let* ((result-set (plist-get data :result))
         (column-index (scootq-column-index result-set (plist-get by :column)))
         (collect-fn (cond
                      ((plist-get keep :column) (lambda (record)
                                                  (aref record (scootq-column-index result-set (plist-get keep :column)))))
                      (t #'identity)))
         (result nil))
    (mapc (lambda (record)
            (let ((group-key (alist-get (aref record column-index) result nil nil #'equal)))
              (setf (alist-get (aref record column-index) result nil nil #'equal) (append group-key (list (funcall collect-fn record))))))
          (alist-get 'rows result-set))
    result))

(cl-defun scootq (&key query connection format target)
  "Run QUERY using CONNECTION and return the result as FORMAT.

TARGET specifies what to do with the result.  It can be a symbol or a plist.

If CONNECTION is nil, the current connection stored in
`scootq--current-connection` is used, which is the expectation of the
`with-scoot-connection macro'."
  (interactive)
  (unless query
    (error "scootq: :query is required"))

  (let ((conn (or connection scootq--current-connection)))
    (unless conn
      (error "scootq: No Scoot connection provided and no active current connection present"))

    (let* ((result (scootq--query-sync conn query))
           (target-plistp (plistp target))
           (target-unless (when target-plistp (plist-get target :unless)))
           (target-type (cond
                         ((symbolp target) target)
                         ((plistp target) (plist-get target :type))
                         (t nil))))
      (unless (and target-unless
                     (functionp target-unless)
                     (funcall target-unless result))
        (pcase target-type
          ('result-buf (scoot--open-resultset (plist-put result :buffer-name (plist-get target :name))))))
      result)))


(provide 'scoot-dsl)
;;; scoot-dsl.el ends here
