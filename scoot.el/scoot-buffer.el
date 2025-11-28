;;; scoot-buffer.el --- summary -*- lexical-binding: t -*-

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

;; Contains the scoot-buffer-mode that is designed to be derived from
;; for all scoot major modes that are tied to a specific connection

;;; Code:

(require 'outline)
(require 'scoot-common)
(require 'scoot-connection)
(require 'scoot-input)
(require 'scoot-table)
(require 'scoot-query-block)



;; Local Variables

(defvar-local scoot-buffer-connection nil
  "The server connection used by the current Scoot Buffer.")

(defvar-local scoot-buffer-outline-sections 0
  "The number of foldable headings currently in the buffer.
Used to enable/disable `outline-minor-mode`.")

(defvar-local scoot-buffer-sections nil
  "The current buffer section layout.")



;; Custom faces

(defface scoot-label-face
  '((t :inherit font-lock-function-name-face))
  "Face used for result headers."
  :group 'scoot)

(defface scoot-outline-header-face
  '((t :inherit outline-1))
  "Face used for foldable outline headers."
  :group 'scoot)



;; Outline Mode Integration

(defun scoot-buffer-toggle-outline ()
  "Invoke `outline-toggle-children` if outline mode is active."
  (interactive)
  (when (bound-and-true-p outline-minor-mode)
    (outline-toggle-children)))

(defun scoot-buffer--activate-outline-minor-mode ()
  "Configure and activate `outline-minor-mode`."
  (setq-local outline-level (lambda () 1)
              outline-search-function #'outline-search-level
              outline-minor-mode-cycle t
              outline-minor-mode-highlight t
              outline-minor-mode-use-buttons 'insert)
  (outline-minor-mode 1))

(defun scoot-buffer--deactivate-outline-minor-mode ()
  "Wipe configuration and deactivate `outline-minor-mode`, if present."
  (dolist (var '(outline-level
                 outline-search-function
                 outline-minor-mode-cycle
                 outline-minor-mode-highlight
                 outline-minor-mode-use-buttons))
    (when (local-variable-p var)
      (kill-local-variable var)))
  (setq scoot-buffer-outline-sections 0)
  (when (bound-and-true-p outline-minor-mode)
    (outline-minor-mode -1)))



;; Buffer Connection Header

(defun scoot-buffer--insert-property-grid (info)
  "Insert a grid of properties displayed as \"Property: Value\".

INFO contains the grid information as a plist with the following properties:
:columns - The number of columns to display
:data - A list of plists containing the properties."
  (let* ((columns (plist-get info :columns))
         (column-min-width 0)
         (entries (mapcar (lambda (entry)
                            (let* ((value (format "%s" (plist-get entry :value)))
                                   (width (length (concat (plist-get entry :label)
                                                          ": "
                                                          value)))
                                   (updated (scoot--plist-merge
                                             entry
                                             (list :value
                                                   value
                                                   :min-width
                                                   width))))
                              (when (> width column-min-width)
                                (setq column-min-width width))
                              updated))
                          (plist-get info :data)))
         (column-width (/ (window-width) (plist-get info :columns))))
    (let ((index 1))
      (mapcar
       (lambda (entry)
         (let* ((meta (plist-get entry :meta))
                (full-cell (concat
                            (propertize (plist-get entry :label)
                                        'role 'data-label
                                        'meta meta
                                        'face 'scoot-label-face)
                            (propertize ": "
                                        'role 'data-label
                                        'meta meta)
                            (propertize (plist-get entry :value)
                                        'role 'data-value
                                        'meta meta)))
                (margin (- column-width (length full-cell)))
                (adjusted (if (<= margin 0)
                              (concat (substring full-cell 0 (+ (length full-cell) margin -4))
                                      (propertize "..." 'face 'font-lock-comment-face))
                            full-cell)))
           (insert adjusted)
           (cond
            ((zerop (% index columns))
             (insert "\n"))
            ((<= margin 0)
             (insert " "))
            ((insert (make-string (- column-width (plist-get entry :min-width)) ?\s))))
           (setq index (1+ index))))
       entries))))

(defun scoot-buffer--insert-connection-header ()
  "Insert buffer connection basic header grid."
  (let ((connection (scoot-context--get-connection (plist-get scoot-buffer-connection :context)
                                                   (plist-get scoot-buffer-connection :name))))

    (scoot-buffer--insert-property-grid
     (list :columns 2
           :data (list (list :label "Connection"
                             :value (plist-get connection :name)
                             :meta 'name)
                       (list :label "Database"
                             :value (plist-get connection :database)
                             :meta 'database)
                       (list :label "Host"
                             :value (plist-get connection :host)
                             :meta 'host)
                       (list :label "Port"
                             :value (plist-get connection :port)
                             :meta 'port)
                       (list :label "Dialect"
                             :value (plist-get connection :dialect)
                             :meta 'dialect)
                       (list :label "Driver"
                             :value (plist-get connection :driver)
                             :meta 'driver))))))



;; Query Block

(defun scoot-buffer--insert-query-block! (section-def)
  "Insert a query block section into the current buffer according to SECTION-DEF."
  (insert (propertize (or (plist-get section-def :title) "Query: ")
                      'face
                      'scoot-label-face))
  (insert "\n")
  (scoot-qb--insert-query-block! (format "%s" (plist-get section-def :current-sql))
                                 (list :on-change (plist-get section-def :on-change))))



;; Table

(defun scoot-buffer--insert-table (section-def)
  "Insert a data table into the current buffer according to SECTION-DEF."
  (scoot-table--insert-table! (plist-get section-def :data)
                              (plist-get section-def :editablep))
  (insert "\n"))



;; Object Type Header

(defun scoot-buffer--insert-object-type-header (section-def)
  "Insert an object type header displaying type and name.

These values are dervied from SECTION-DEF."
  (let* ((data (plist-get section-def :data))
         (type (plist-get data :object-type))
         (name (plist-get data :object-name)))
    (insert (propertize (concat (pcase type
                                  ('table "Table")
                                  ('schema "Schema")
                                  ('database "Database"))
                                ": ")
                        'face 'scoot-label-face))
    (insert (propertize name
                        'thing (intern (concat (symbol-name type)
                                               "-name"))))
    (insert "\n")))



;; DDL

(defun scoot-buffer--insert-ddl (section-def)
  "Insert a foldable DDL section according to SECTION-DEF."
  (when-let ((ddl (alist-get 'sql (plist-get section-def :data))))
    (dolist (sql-entry ddl)
      (insert (propertize (car sql-entry)
                          'face 'scoot-outline-header-face
                          'outline-level 1))
      (insert "\n")
      (scoot--insert-propertized-string
       (scoot--propertize-sql
        (cdr sql-entry)))
      (insert "\n")
      (setq-local scoot-buffer-outline-sections (1+ scoot-buffer-outline-sections)))))



;; Render Functions

(defun scoot-buffer--render-section (section-def)
  "Render the content section described by SECTION-DEF."
  (let* ((section-def (if (symbolp section-def) (list :type section-def) section-def))
         (section-type (plist-get section-def :type)))
    (pcase section-type
      ('connection-header (scoot-buffer--insert-connection-header))
      ('object-type-header (scoot-buffer--insert-object-type-header section-def))
      ('query-editor (scoot-buffer--insert-query-block! section-def))
      ('data-table (scoot-buffer--insert-table section-def))
      ('ddl-outline (scoot-buffer--insert-ddl section-def)))
    (insert "\n")))


(defun scoot-buffer--render ()
  "Render the buffer contents as described by `scoot-buffer-sections`."
  (dolist (section-def scoot-buffer-sections)
    (scoot-buffer--render-section section-def))
  (scoot-buffer--editable nil)
  (scoot--restore-cursor))


(defun scoot-buffer--editable (editablep)
  "Toggle the read-only status of the current buffer.

Truthy values of EDITABLEP will open the buffer for modification, and
falsy values will enforce `read-only-mode` and scoot invariants thereof."
  (if editablep
      (progn
        (scoot-buffer--deactivate-outline-minor-mode)
        (read-only-mode -1))
    (progn
      (unless (zerop scoot-buffer-outline-sections)
        (scoot-buffer--activate-outline-minor-mode)
        (save-excursion
          (goto-char (point-max))
          (outline-hide-subtree)))
      (read-only-mode 1))))

(defun scoot-buffer--clear ()
  "Clear the current buffer contents and disable any modes that may interfere."
  (scoot--save-cursor)
  (scoot-buffer--editable t)
  (scoot-widget--destroy-widgets!)
  (erase-buffer)
  (setq-local scoot-buffer-outline-sections 0))

(defun scoot-buffer-refresh ()
  "Redraw/Refresh the buffer contents."
  (interactive)
  (scoot-buffer--clear)
  (scoot-buffer--render))



;; Scoot Buffer Mode - scoot-buffer-mode

(defvar scoot-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'scoot-buffer-refresh)
    (define-key map (kbd "h") #'describe-mode)
    (define-key map (kbd "?") #'describe-mode)
    (define-key map (kbd "TAB") #'scoot-buffer-toggle-outline)
    map)
  "Base keymap for modes dervied from `scoot-buffer-mode`.")

(define-derived-mode scoot-buffer-mode fundamental-mode "Scoot Buffer"
  "Base Major Mode for non-file buffer Scoot modes.

This mode provides basic interaction hooks and buffer-local variables."
  ;; Setup resolvers for buffer
  (setq-local scoot-local--context-name-resolvers '((lambda () (plist-get scoot-buffer-connection :context))))
  (setq-local scoot-local--connection-resolvers '((lambda () scoot-buffer-connection)))
  (setq-local scoot-local--table-name-resolvers '(scoot-connection--table-prompt))

  (setq-local truncate-lines t)
  (setq buffer-read-only t))



(provide 'scoot-buffer)

;;; scoot-buffer.el ends here
