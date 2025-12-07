;;; scoot-input.el --- summary -*- lexical-binding: t -*-

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

;; commentary

;;; Code:

(require 'scoot-widget)



;; Shadow buffer management method implementations

(cl-defmethod scoot-widget--shadow-buffer-name ((_widget-type (eql 'input))
                                                _widget-name)
  "Construct the shadow buffer name for input WIDGET-NAME."
  (format " *scoot shadow-input(%s)*" (buffer-name)))

(cl-defmethod scoot-widget--shadow-after-change-hook ((_widget-type (eql 'input))
                                                      _widget-name)
  "Get the hook to run after the shadow buffer content changes."
  #'scoot-input--shadow-after-change-hook)



;; Input display functions

(defun scoot-input--format-content (value)
  "Format the cell VALUE as an input widget."
  (let ((face (get-text-property (point) 'face)))
    (propertize value
                'face `(:inherit ,face :background "#000000"))))


(defun scoot-input--replace-content! (input value)
  ""
  (let ((begin (marker-position (plist-get input :widget-start)))
        (end (marker-position (plist-get input :widget-end)))
        (align (plist-get input :align))
        (margin (make-string (max 0
                                  (- (plist-get input :widget-min-width)
                                     (length value)))
                             ?\s)))
    (delete-region begin end)
    (goto-char begin)
    (when (eq align 'right)
      (insert margin))
    (set-marker (plist-get input :editable-start) (point))
    (insert (scoot-input--format-content value))
    (set-marker (plist-get input :editable-end) (point))
    (when (eq align 'left)
      (insert margin))
    (set-marker (plist-get input :widget-end) (point))))


(cl-defun scoot-input--install-input! (&key begin end column type formatter record record-cell resize-hook remove-hook)
  "Install an input spanning from BEGIN to END.

FORMATTER will be used to format the value of RECORD according to its
display behavior.  Will be pre-populated with the current column value
and enforce the constraints of TYPE.

COLUMN may be supplied to allow applying column-level constraints.

RESIZE-HOOK and REMOVE-HOOK can be provided for non-standard behavior
editing forces a resize of the widget bounds and when the input is
removed, respectively."
  (goto-char begin)
  (scoot-widget--create
      :type 'input
      :name 'input
      :editable-region 'value
      :value (plist-get record-cell :formatted-value)

      :init
      (progn
        (plist-put widget :formatter formatter)
        (plist-put widget :record record)
        (plist-put widget :record-cell record-cell)
        (plist-put widget :contain-cursor t)
        (plist-put widget :data-type type)

        (plist-put widget :align (plist-get formatter :align))
        (plist-put widget :widget-min-width (- end begin))
        (plist-put widget :remove-on-teardown t)

        (plist-put widget :editable-start (copy-marker begin))
        (plist-put widget :widget-end (copy-marker end t))
        (plist-put widget :editable-end (copy-marker end))

        (scoot-input--replace-content! widget value)

        (when resize-hook
          (plist-put widget :resize-hook resize-hook))
        (when remove-hook
          (plist-put widget :remove-hook remove-hook))
        (when column
          (plist-put widget :column column)))

      :finalize
      (progn
        (with-scoot-widget-shadow-buffer widget
          (goto-char (point-max)))
        (scoot-input-mode 1))))

(defun scoot-input--refresh-input! ()
  "Refresh the active input with the contents from the shadow buffer."
  (condition-case err
      (let* ((inhibit-read-only t)
             (inhibit-modification-hooks t)
             (widget (scoot-widget--get-widget :type 'input :name 'input))
             (current-length (- (plist-get widget :widget-end)
                                (plist-get widget :widget-start)))
             (new-value (scoot-widget--shadow-buffer-content widget)))

        (scoot-input--replace-content! widget new-value)

        (let ((new-length (- (plist-get widget :widget-end)
                             (plist-get widget :widget-start))))

          (when (/= current-length new-length)
            (when-let ((resize-hook (plist-get widget :resize-hook)))
              (funcall resize-hook new-length)))))
    (error
     (message "Error while refreshing input field: %s - %s" (car err) (cdr err)))))



;; Type validation & constraint enforcing

(defun scoot-input--enforce-string-constraints (type-spec value _beg end _len)
  "Enforce the String VALUE according to TYPE-SPEC.

BEG, END and LEN describe the change that has occurred in the shadow
buffer."
  (when-let* ((max-length (alist-get 'max-len type-spec))
              (overshoot (- (length value) max-length)))
    (when (> overshoot 0)
      (delete-region (- end overshoot) end)
      (goto-char (- end overshoot)))))

(defun scoot-input--enforce-integer-constraints (_type-spec value beg end _len)
  "Enforce the Integer VALUE according to TYPE-SPEC.

BEG, END and LEN describe the change that has occurred in the shadow
buffer."
  (when (not (string-match "^-?[0-9]*$" value))
    (delete-region beg end)
    (goto-char beg))
  (when (length= value 0)
    (insert "0")))

(defun scoot-input--validate-change (type-spec beg end len)
  "Validate the change in the shadow buffer according to TYPE-SPEC.

BEG, END and LEN describe the change that has occurred in the shadow
buffer."
  (let ((value (buffer-string))
        (inhibit-read-only t))
    (pcase (alist-get 'type type-spec)
      ("STRING" (scoot-input--enforce-string-constraints type-spec value beg end len))
      ("INTEGER" (scoot-input--enforce-integer-constraints type-spec value beg end len))
      (_ nil))))



;; Commands

(defun scoot-input--cancel-input-mode ()
  "Exit scoot-input-mode and restore the widget area."
  (interactive)
  (let ((widget (scoot-widget--get-widget :type 'input :name 'input)))
    (when-let ((remove-hook (plist-get widget :remove-hook)))
      (funcall remove-hook widget))
    (scoot-input-mode -1)))

(defun scoot-input--confirm-edit ()
  "Confirm the changes made and exit the input mode."
  (interactive)
  (let* ((widget (scoot-widget--get-widget :type 'input :name 'input))
         (cell (plist-get widget :record-cell))
         (formatter (plist-get widget :formatter))
         (new-value (scoot-widget--shadow-buffer-content widget))
         (current-value (plist-get cell :value))
         (original-value (plist-get cell :original-value)))
    (cond
     ((not original-value)
      (when (not (equal (format "%s" new-value) (format "%s" current-value)))
        (plist-put cell :original-value current-value)))

     ((equal (format "%s" new-value) (format "%s" original-value))
      (scoot--plist-remove! cell :original-value)))

    (plist-put cell :value new-value)
    (plist-put cell :formatted-value (funcall (plist-get formatter :format-value) new-value))
    (scoot-input--cancel-input-mode)))

(defun scoot-input--cycle-integer (widget n)
  "Cycle the Integer value at point of WIDGET by N."
  (let* ((pos (- (point) (marker-position (plist-get widget :editable-start))))
         (buf-value (scoot-widget--shadow-buffer-content widget))
         (digit (max 0 (1- (- (length buf-value) pos)))))
    (with-scoot-widget-shadow-buffer widget
      (let ((offset (- (length buf-value) (point)))
            (new-value (format "%s" (+ (string-to-number buf-value)
                                       (* n (expt 10 digit))))))
        (with-silent-modifications
          (erase-buffer))
        (insert new-value)
        (goto-char (- (length new-value) offset))))))

(defun scoot-input--cycle-string (widget n)
  "Cycle the String value at point of WIDGET by N.

Requires an enum-like check constraint on the column."
  (when-let ((enum-values (alist-get 'rhs
                                     (alist-get 'condition
                                                (seq-find (lambda (c)
                                                            (equal (alist-get 'type c) "chk"))
                                                          (alist-get 'constraints
                                                                     (plist-get widget :column)))))))
    (with-scoot-widget-shadow-buffer widget
      (let* ((pos (or (cl-position (buffer-string)
                                   enum-values
                                   :test #'string=)
                      (- 0 n)))
             (new-value (aref enum-values (mod (+ pos n) (length enum-values)))))
        (erase-buffer)
        (insert new-value)))))

(defun scoot-input--cycle-value (n)
  "Cycle the value of the input by N."
  (let* ((widget (scoot-widget--get-widget :type 'input :name 'input))
         (type-spec (plist-get widget :data-type)))
    (pcase (alist-get 'type type-spec)
      ("INTEGER" (scoot-input--cycle-integer widget n))
      ("STRING" (scoot-input--cycle-string widget n))
      (_ nil))))

(defun scoot-input--cycle-value-up ()
  "Cycle the value of the input."
  (interactive)
  (scoot-input--cycle-value 1))

(defun scoot-input--cycle-value-down ()
  "Cycle the value of the input."
  (interactive)
  (scoot-input--cycle-value -1))



;; Hooks

(defun scoot-input--shadow-after-change-hook (widget beg end len)
  "Run after modification happens in shadow buffer of WIDGET.

BEG, END and LEN detail the beginning, end and length of the change."
  (when-let* ((type-spec (with-current-buffer scoot-widget-display-buffer
                           (plist-get widget
                                      :data-type))))
    (scoot-input--validate-change type-spec beg end len))
  (with-current-buffer scoot-widget-display-buffer
    (scoot-input--refresh-input!)))



;; Scoot Input Mode

(defvar scoot-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'scoot-input--confirm-edit)
    (define-key map (kbd "C-g") 'scoot-input--cancel-input-mode)
    (define-key map (kbd "<up>") 'scoot-input--cycle-value-up)
    (define-key map (kbd "<down>") 'scoot-input--cycle-value-down)
    map))

(define-minor-mode scoot-input-mode
  "Minor mode for input widgets."
  :lighter " Input"
  :keymap scoot-input-mode-map

  (if scoot-input-mode
      (scoot-widget--setup-input-mode 'input 'input)
    (scoot-widget--teardown-input-mode 'input 'input)))

(provide 'scoot-input)

;;; scoot-input.el ends here
