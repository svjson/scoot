;;; scoot-xref.el --- summary -*- lexical-binding: t -*-

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

;; Adds a generic xref-backend that provides hooks for the various scoot modes
;; to jack into to provide quick object lookup using xref..

;; commentary

;;; Code:

(require 'dash)
(require 'xref)
(require 'cl-lib)
(require 'scoot-common)

(defvar-local scoot-xref-action-fn nil
  "Function to handle xref actions, specified locally by each buffer.")

(defvar-local scoot-xref-identifier-at-point-fn nil
  "Function to determine identifier-at-point, specified locally by each buffer.")

(defvar-local scoot-xref-completion-provider-fn nil
  "Function to provide buffer-local completion alternatives.")

(defvar scoot-xref-async-cache (make-hash-table :test 'equal))

(defcustom scoot-xref-database-icon " "
  "Icon used to prefix databases in xref identifiers.

Should be unique among scoot-xref icons"
  :type 'string
  :group 'scoot)

(defcustom scoot-xref-table-icon " "
  "Icon used to prefix tables in xref identifiers.

Should be unique among scoot-xref icons"
  :type 'string
  :group 'scoot)

(defcustom scoot-xref-column-icon " "
  "Icon used to prefix columns in xref identifiers.

Should be unique among scoot-xref icons"
  :type 'string
  :group 'scoot)

(defcustom scoot-xref-expr-icon " "
  "Icon used to prefix expressions in xref identifier.

Should be unique among scoot-xref icons"
  :type 'string
  :group 'scoot)

(defun scoot-xref--database-identifier (db)
  "Construct a database identifier recognized by scoot-xref from DB."
  (concat (propertize
           (copy-sequence scoot-xref-database-icon)
           'face 'org-warning)
          (plist-get db :name)))

(defun scoot-xref--table-identifier (tbl)
  "Construct a table identifier recognized by scoot-xref from TBL."
  (concat (propertize
           (copy-sequence scoot-xref-table-icon)
           'face 'font-lock-keyword-face)
          (plist-get tbl :name)))

(defun scoot-xref--column-identifier (col)
  "Construct a column identifier recognized by scoot-xref from COL."
  (let* ((table (plist-get col :table))
         (table-identifier (when table (scoot-xref--table-identifier
                                        (list :name table)))))
    (concat
     (when table-identifier
       (concat table-identifier (propertize " / " 'face 'window-divider)))
     (propertize
      (copy-sequence scoot-xref-column-icon)
      'face 'font-lock-constant-face)
     (plist-get col :column))))

(defun scoot-xref--expr-identifier (exprl)
  "Construct an expression identifier recognized by scoot-xref from EXPRL.

EXPRL is expected to be a list of plists."
  (concat
   (propertize
    (copy-sequence scoot-xref-expr-icon)
    'face 'font-lock-function-name-face)
   (string-join (mapcar
                 (lambda (expr)
                   (concat
                    (plist-get expr :lhs)
                    (pcase (plist-get expr :type)
                      ('equals "=")
                      (_ "?"))
                    (format "%s" (plist-get expr :rhs))))
                 exprl)
                ",")))

(defun scoot-xref--record-reference-identifier (ref)
  "Construct a record reference identifier recognized by scoot-xref from REF."
  (let* ((table (plist-get ref :table))
         (columns (plist-get ref :columns))
         (table-identifier (scoot-xref--table-identifier
                            (list :name table))))
    (concat
     table-identifier
     (propertize " / " 'face 'window-divider)
     (scoot-xref--expr-identifier (mapcar
                                   (lambda (col-name)
                                     (list :type 'equals
                                           :lhs col-name
                                           :rhs (plist-get ref :value)))
                                   columns)))))

(defun scoot-xref--destructure-expr (expr-str)
  "Destructure expression segment EXPR-STR into :lhs, :oper and :rhs."
  (mapcar
   (lambda (expr)
     (let ((match (string-match "\\(.*?\\)\\([=!<>]+\\)\\(.*\\)" expr)))
       (when match
         (list :lhs (match-string 1 expr)
               :oper (match-string 2 expr)
               :rhs (match-string 3 expr)))))
   (string-split expr-str",")))

(defun scoot-xref--destructure-xref (xref)
  "Destructure an XREF symbol and identify its parts."
  (let* ((segments
          (mapcar
           (lambda (seg)
             (let* ((typec (cond
                            ((string-prefix-p scoot-xref-database-icon seg) :database)
                            ((string-prefix-p scoot-xref-column-icon seg) :column)
                            ((string-prefix-p scoot-xref-table-icon seg) :table)
                            ((string-prefix-p scoot-xref-expr-icon seg) :expr))))
               (cons typec (cond
                            ((eq typec :database) (substring seg (length scoot-xref-database-icon)))
                            ((eq typec :column) (substring seg (length scoot-xref-column-icon)))
                            ((eq typec :table) (substring seg (length scoot-xref-table-icon)))
                            ((eq typec :expr) (scoot-xref--destructure-expr
                                               (substring seg (length scoot-xref-expr-icon))))
                            (t seg)))))
           (split-string xref " / ")))
         (result (list :xref xref
                       :target (caar (last segments)))))
    (dolist (seg segments)
      (plist-put result (car seg) (cdr seg)))
    result))

(defun scoot-xref--get-conn-cache (connection)
  "Get the xref lookup cache for CONNECTION.  Will be created it if uninitialized."
  (let ((conn-name (if (stringp connection) connection (plist-get connection :name))))
    (or (gethash conn-name scoot-xref-async-cache)
        (puthash conn-name (make-hash-table :test 'equal) scoot-xref-async-cache))))

(cl-defgeneric scoot-xref--point-is-thing-p (type point xref-target props)
  "Test POINT PROPS against specification in XREF-TARGET, dispatch on TYPE.")

(defun scoot-xref--buffer-point-is-p (buf point xref-target)
  "Test POINT in BUF against xref spec in XREF-TARGET."
  (with-current-buffer buf
    (scoot-xref--point-is-thing-p (-> xref-target
                                      (plist-get :target)
                                      (symbol-name)
                                      (substring 1)
                                      (intern))
                                  point
                                  xref-target
                                  (scoot--props-at point))))

(defun scoot-xref--find-buffer-location (buf xref-target)
  "Find the location of XREF-TARGET in BUF."
  (with-current-buffer buf
    (let ((end-p nil)
          (point (point-min)))
      (while (and (not end-p)
                  (not (null point))
                  (<= point (point-max)))
        (setq point (next-single-property-change point 'thing nil))
        (setq end-p (and point
                         (scoot-xref--buffer-point-is-p buf point xref-target))))
      point)))

(defun scoot-xref--is-valid-xref (xref target)
  "Verify that the xref-item XREF is still valid for TARGET."
  (when (and xref (xref-item-p xref))
    (when-let* ((loc (xref-item-location xref))
                (buf (xref-buffer-location-buffer loc))
                (_ (buffer-live-p buf))
                (_ (scoot-xref--buffer-point-is-p buf (xref-buffer-location-position loc) target)))
      t)))

(defun scoot--xref-backend ()
  "Xref backend for scoot-result-mode."
  'scoot-modes-xref)

(cl-defstruct scoot-xref-async-location
  "Location struct for xref supporting async lookup."
  identifier
  marker
  pending)

(cl-defmethod xref-location-marker ((loc scoot-xref-async-location))
  "Location marker resolution for async xref lookups.

Lookups where LOC is an instance of `scoot-xref-async-location` will
end up here and simply return a `point-marker` if the async operation is
still pending."
  (if (scoot-xref-async-location-pending loc)
      (point-marker)
    (scoot-xref-async-location-marker loc)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql scoot-modes-xref)))
  "Return the identifier using buffer properties."
  (when (and scoot-xref-identifier-at-point-fn
             (symbol-function scoot-xref-identifier-at-point-fn))
    (funcall scoot-xref-identifier-at-point-fn)))

(cl-defmethod xref-backend-definitions ((_backend (eql scoot-modes-xref)) identifier)
  "Dispatch IDENTIFIER to appropriate action based on context."
  (when (and identifier
             scoot-xref-action-fn
             (symbol-function scoot-xref-action-fn))
    (funcall scoot-xref-action-fn (scoot-xref--destructure-xref identifier))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql scoot-modes-xref)))
  "Identifiers to show when xref launches a prompt for identifiers."
  (when (and scoot-xref-completion-provider-fn
             (symbol-function scoot-xref-completion-provider-fn))
    (funcall scoot-xref-completion-provider-fn)))

(provide 'scoot-xref)

;;; scoot-xref.el ends here
