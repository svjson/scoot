;;; scoot-query-block.el --- summary -*- lexical-binding: t -*-

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

;; This file provides a minor mode for editing textarea-like blocks of
;; SQL code, as used by scoot-resultset-mode

;;; Code:

(require 'simple)
(require 'scoot-common)
(require 'scoot-widget)


;; Variables

(defvar-local scoot-query-block-start nil
  "The start position of the query block in the current buffer.")

(defvar-local scoot-query-block-end nil
  "The end position of the query block in the current buffer.")

(defvar-local scoot-query-block-content nil
  "Snapshot of the current contents of the query-block.")

(defvar-local scoot-query-block-shadow-buffer nil
  "Reference to the shadow buffer backing the query block.")



;; Custom faces

(defface scoot-query-block-face
  '((t :inherit highlight :background "#222222" :extend nil))
  "Face used fo query blocks."
  :group 'scoot)

(defface scoot-query-block-padding-face
  '((t :inherit scoot-query-block-face :extend nil))
  "Face used fo query blocks."
  :group 'scoot)



;; Shadow buffer management method implementations

(cl-defmethod scoot-widget--shadow-buffer-name ((_widget-type (eql 'query-block))
                                                _widget-name)
  "Construct the shadow buffer name for input WIDGET-NAME."
  (format " *scoot shadow-qb(%s)*" (buffer-name)))

(cl-defmethod scoot-widget--get-shadow-buffer ((_widget-type (eql 'query-block))
                                               _widget-name)
  "Get the shadow buffer for the query block."
  scoot-query-block-shadow-buffer)

(cl-defmethod scoot-widget--set-shadow-buffer ((_widget-type (eql 'query-block))
                                               _widget-name
                                               buffer)
  "Set the shadow buffer for the active input to BUFFER."
  (setq scoot-query-block-shadow-buffer buffer))

(cl-defmethod scoot-widget--shadow-after-change-hook ((_widget-type (eql 'query-block))
                                                      _widget-name)
  "Return the after-change-hook for query-block widgets."
  #'scoot-qb--shadow-after-change-hook)



;; Query block display functions

(defun scoot-qb--build-query-block (query &optional opts)
  "Build and propertize query block contents from QUERY into a string.

This function relies on `window-body-width` to determine the
width of the query-block unless OPTS is provided with a value
for :width."
  (let* ((width (or (plist-get opts :width) (window-body-width)))
         (wrapped-lines (scoot--wrap-string query width))
         (content ""))
    (dolist (line wrapped-lines)
      (setq content (concat content
                            (propertize line
                                        'face 'scoot-query-block-face
                                        'thing 'query-block)
                            (propertize (make-string
                                         (max 0 (1- (- width (length line)) ))
                                         ?\s)
                                        'face 'scoot-query-block-padding-face
                                        'thing 'query-block
                                        'qb-padding t)
                            "\n")))
    content))

(defun scoot-qb--insert-query-block (query &optional opts)
  "Insert QUERY string with wrapping and box-like styling.

This function relies on `window-body-width` to determine the
width of the query-block unless OPTS is provided with a value
for :width.

This should be called when initially setting up a query block.
For subsequent updates/refreshes of the query block, call
`scoot-qb--refresh-query-block`."
  (let* ((width (or (plist-get opts :width) (window-body-width)))
         (wrapped-lines (scoot--wrap-string query width))
         (widget (scoot-widget--get-widget-config 'query-block
                                                  'query-block)))
    (setq-local scoot-query-block-start (point))
    (plist-put widget :editable-start (copy-marker (point)))
    (plist-put widget :widget-start (copy-marker (point)))
    (plist-put widget :widget-start-line (line-number-at-pos (point)))
    (scoot-widget--init-shadow-buffer 'query-block
                                      'query-block
                                      (string-join wrapped-lines "\n"))
    (insert (scoot-qb--build-query-block (scoot-qb--get-query widget) opts))
    (setq-local scoot-query-block-end (point))
    (plist-put widget :editable-end (copy-marker (point)))
    (plist-put widget :widget-end (copy-marker (point)))
    widget))

(defun scoot-qb--refresh-query-block ()
  "Redraw the query-block with the contents of the shadow buffer."
  (scoot--save-cursor)
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (delete-region scoot-query-block-start
                     scoot-query-block-end)
      (goto-char scoot-query-block-start)
      (insert (scoot-qb--build-query-block
               (with-current-buffer scoot-query-block-shadow-buffer
                 (buffer-string))))
      (setq-local scoot-query-block-end (point))
      (let ((widget (scoot-widget--get-widget-config 'query-block 'query-block)))
        (set-marker (plist-get widget :widget-start) scoot-query-block-start)
        (set-marker (plist-get widget :editable-start) scoot-query-block-start)
        (set-marker (plist-get widget :widget-end) scoot-query-block-end)
        (set-marker (plist-get widget :editable-end) scoot-query-block-end))))
  (scoot--restore-cursor))



;; Query Block interaction functions

(defun scoot-qb--query-block-at-p (pt)
  "Return non-nil if point PT is inside the query block."
  (eq (alist-get 'thing (scoot--props-at pt)) 'query-block))

(defun scoot-qb--query-block-at-point-p ()
  "Return non-nil if the cursor is inside the query block."
  (scoot-qb--query-block-at-p (point)))

(defun scoot-qb--enter-query-block ()
  "Synchronize cursor positions when entering the visible query block."
  (let ((qb-pos (scoot-widget--point->position (scoot-widget--get-widget-config 'query-block
                                                                        'query-block)
                                               (point)
                                               (list :absolute-p t
                                                     :shadow-p t)))
        (updated-pos nil))
    (with-current-buffer scoot-query-block-shadow-buffer
      (scoot-qb--move-to-qb-pos qb-pos)
      (setq updated-pos (list :line (line-number-at-pos) :col (current-column))))
    (scoot-qb--move-to-qb-pos updated-pos scoot-query-block-start)))

(defun scoot-qb--move-to-qb-pos (pos &optional initial-pos)
  "Move the cursor to POS inside the query block, if possible.

INITIAL-POS allows overriding the default of (point-min)."
  (goto-char (or initial-pos (point-min)))
  (forward-line (1- (plist-get pos :line)))
  (forward-char (min (plist-get pos :col) (- (line-end-position) (line-beginning-position)))))

(defun scoot-qb--get-query (qb-widget)
  "Return the query currently entered into the query block QB-WIDGET."
  (with-current-buffer (scoot-widget--get-shadow-buffer (plist-get qb-widget :type)
                                                        (plist-get qb-widget :name))
    (buffer-substring-no-properties (point-min) (point-max))))



;; Hooks

(defun scoot-qb--shadow-after-change-hook (_ _ _)
  "Run after modification happens in shadow buffer.

BEG, END and LEN detail the beginning, end and length of the change."
  (with-current-buffer scoot-widget-display-buffer
    (scoot-qb--refresh-query-block)))


;; Query Block Mode

(declare-function scoot-rs--execute-query "scoot-resultset")

(defvar scoot-query-block-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") #'scoot-rs--execute-query)
    map)
  "Keymap for `scoot-query-block-mode'.")

(define-minor-mode scoot-query-block-mode
  "Minor mode for editing and managing the query-block in Scoot Result Mode."
  :lighter " QueryBlock"
  :keymap scoot-query-block-mode-map

  (if scoot-query-block-mode
      (progn
        (scoot-widget--setup-input-mode 'query-block 'query-block)
        (read-only-mode -1)
        (scoot-qb--enter-query-block))
    (progn
      (read-only-mode 1)
      (scoot-widget--teardown-input-mode 'query-block 'query-block))))


(provide 'scoot-query-block)

;;; scoot-query-block.el ends here

