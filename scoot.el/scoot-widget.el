;;; scoot-widget.el --- summary -*- lexical-binding: t -*-

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

;; This file contains common features for Scoot widgets, such as the editable
;; query block and editable table cells.

;;; Code:

(require 'cl-lib)
(require 'scoot-common)



;; Variables

(defvar-local scoot-widget-display-buffer nil
  "Reference from a shadow buffer to the main display buffer.")

(defvar-local scoot-original-local-map nil
  "Reference to the original local keymap before enabling scoot-input-mode.")

(defvar-local scoot--active-widgets nil)

(defvar-local scoot--pre-command-point nil)



;; Widget creation

(cl-defmacro scoot-widget--create (&key name
                                        type
                                        value
                                        editable-region
                                        on-change-function
                                        opts
                                        init
                                        finalize)
  "Create a widget instance of TYPE with NAME.

Widgets are tracked in the buffer-local alist `scoot--active-widgets`.

Widgets are identified by TYPE and NAME either as separate variables
or a compound symbol in the format of <TYPE>--<NAME>.

This macro sets up all common properties and facilities that concern
widgets.  Rendering is performed by the code supplied in INIT and FINALIZE.

The start position of the widget in the buffer contents are set from
point when the widget rendering begins and the end position is set from
point when the widget rendering is complete, ie when the INIT block
exits.

Keys:
NAME - symbol - An identifying name, unique per TYPE and buffer.
TYPE - symbol - Widget type.
VALUE - Widget-specific initial value.
EDITABLE-REGION - Controls which part of the rendered widget that is
                  to be editable.
                  Valid values are:
                  - `widget - The entire widget area.
ON-CHANGE-FUNCTION - Optional callback that is invoked when the value
                     controlled by the widget changes.
OPTS - A let-form of values used to set or override values during
       widget initialization.
INIT - A code block that is expected to render the widget and set up
       additional state.
FINALIZE - A code block that executes when rendering and widget set up
           is complete."
  `(let* ((inhibit-read-only t)
          ,@opts
          (widget (scoot-widget--register-widget! ,type ,name))
          (widget-start-pos (point))
          (widget-start-marker (copy-marker widget-start-pos))
          (widget-end-pos nil)
          (widget-end-marker nil)
          (edit-region ,editable-region)
          (value ,value))
     (unless widget
       (error "Failed to create %s widget %s" ,type ,name))
     (plist-put widget :widget-start widget-start-marker)
     (plist-put widget :widget-start-line (line-number-at-pos widget-start-pos))

     ,init

     (setq widget-end-pos (point))
     (setq widget-end-marker (copy-marker widget-end-pos))
     (plist-put widget :widget-end widget-end-marker)

     (when edit-region
       (plist-put widget :editable-start (pcase edit-region
                                           ('widget (copy-marker widget-start-marker))
                                           ('value (plist-get widget :editable-start))
                                           (_ (error "Invalid region: %s" edit-region))))
       (plist-put widget :editable-end (pcase edit-region
                                         ('widget (copy-marker widget-end-marker))
                                         ('value (plist-get widget :editable-end))
                                         (_ (error "Invalid region: %s" edit-region))))
       (scoot-widget--init-shadow-buffer ,type
                                         ,name
                                         value))
     (plist-put widget :on-change-hook ,on-change-function)
     (add-text-properties widget-start-pos
                          widget-end-pos
                          (list 'cursor-sensor-functions
                                scoot-widget--cursor-sensor-functions))
     ,finalize

     (cursor-sensor-mode 1)
     widget))



;; Shadow buffer management

(cl-defgeneric scoot-widget--shadow-buffer-name (widget-type widget-name)
  "Construct the shadow buffer name for WIDGET-TYPE with WIDGET-NAME.")

(cl-defun scoot-widget--get-shadow-buffer (&key type name identity widget)
  "Get the shadow buffer of a widget.

The widget can be identified by WIDGET instance, IDENTITY symbol or a
combination of TYPE and NAME."
  (plist-get (or widget (scoot-widget--get-widget :type type
                                                  :name name
                                                  :identity identity))
             :shadow-buffer))

(cl-defun scoot-widget--set-shadow-buffer! (&key type name identity widget buffer)
  "Set the shadow buffer of a widget to BUFFER.

The widget can be identified by instance WIDGET, IDENTITY or a combination
of TYPE and NAME."
  (let ((widget (or widget (scoot-widget--get-widget :type type
                                                     :name name
                                                     :identity identity))))
    (if (plist-get widget :shadow-buffer)
        (setf (plist-get widget
                         :shadow-buffer)
              buffer)
      (plist-put widget :shadow-buffer buffer))))

(cl-defgeneric scoot-widget--shadow-after-change-hook (widget-type widget-name)
  "Return a reference to function to run when the shadow buffer changes.

The method delegates on WIDGET-TYPE and may optionally use WIDGET-NAME to
determine the update method.

The returned handler function must take arguments WIDGET, BEG, END and LEN.")



;; Shadow buffer control functions

(defmacro with-scoot-widget-shadow-buffer (widget &rest body)
  "Macro that executes BODY with the shadow buffer of WIDGET as current buffer."
  (declare (indent 1))
  `(with-current-buffer (scoot-widget--get-shadow-buffer :widget ,widget)
     ,@body))

(defun scoot-widget--is-shadow-buffer-p (&optional buffer)
  "Tests if BUFFER or the current buffer is a shadow buffer."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-live-p scoot-widget-display-buffer)))

(defun scoot-widget--init-shadow-buffer (widget-type
                                         widget-name
                                         content)
  "Initialize the shadow buffer containing the actual editable widget content.

WIDGET-TYPE is used to delegate to the correct widget-implementation, which in
turn may or may not use WIDGET-NAME to further specialize the shadow buffer.

CONTENT should be a single string formatted with \"\\n\" as line separators,
if allowed/appplicable.

The resulting buffer is an invisible buffer containing the editable content,
and is used to execute editing and movement commands without the need to
take the padding and property rules of the display buffer into consideration.

The buffer is assigned to a presumably buffer-local variable of the
buffer from which this function was invoked, chosen by the WIDGET-TYPE
widget implementation, which should typically be `scoot-resultset-mode`
buffer."
  (let ((parent-buf (current-buffer))
        (shadow-buffer (scoot-widget--get-shadow-buffer :type widget-type
                                                        :name widget-name)))
    (unless (and shadow-buffer
                 (buffer-live-p shadow-buffer))

      (setq shadow-buffer (get-buffer-create
                           (scoot-widget--shadow-buffer-name widget-type
                                                             widget-name)))
      (scoot-widget--set-shadow-buffer! :type widget-type
                                        :name widget-name
                                        :buffer shadow-buffer)

      (with-current-buffer shadow-buffer
        (setq-local scoot-widget-display-buffer parent-buf)
        (setq-local tab-width (buffer-local-value 'tab-width parent-buf))
        (setq-local indent-tabs-mode (buffer-local-value 'indent-tabs-mode parent-buf))

        (add-hook 'after-change-functions (lambda (beg end len)
                                            (funcall (scoot-widget--shadow-after-change-hook
                                                      widget-type
                                                      widget-name)
                                                     (with-current-buffer scoot-widget-display-buffer
                                                       (scoot-widget--get-widget :type widget-type
                                                                                 :name widget-name))
                                                     beg end len))
                  nil t))
      (add-hook 'kill-buffer-hook (lambda ()
                                    (scoot-widget--kill-shadow-buffer
                                     (scoot-widget--get-shadow-buffer :type widget-type
                                                                      :name widget-name)))
                nil t))

    (scoot-widget--reset-shadow-buffer! shadow-buffer content)
    shadow-buffer))

(defun scoot-widget--reset-shadow-buffer! (shadow-buffer content)
  "Clear and reset the SHADOW-BUFFER contents.

CONTENT should be a single string formatted with \"\\n\" as line separators."
  (with-current-buffer shadow-buffer
    (scoot--save-cursor)
    (with-silent-modifications
      (erase-buffer)
      (insert content))
    (scoot--restore-cursor)))



;; Widget query functions

(cl-defun scoot-widget--at-point (&key point type)
  "Get the first encountered widget at POINT or point.

The search may be limited to widgets of TYPE."
  (let ((p (or point (point))))
    (cdr (seq-find
          (lambda (entry)
            (let* ((w (cdr  entry))
                   (reg (scoot-widget--region w)))
              (and (or (null type)
                       (equal type
                              (plist-get w :type)))
                   (>= p (car reg))
                   (< p (cdr reg)))))
          scoot--active-widgets))))

(defun scoot-widget--region (widget)
  "Get the start and end point of WIDGET."
  (cons (marker-position (plist-get widget :widget-start))
        (marker-position (plist-get widget :widget-end))))

(defun scoot-widget--editable-region (widget)
  "Get the start and end point of WIDGET."
  (when-let ((edit-start (plist-get widget :editable-start))
             (edit-end (plist-get widget :editable-end)))
    (cons (marker-position edit-start)
          (marker-position edit-end))))

(defun scoot-widget--buffer-string (widget)
  "Get the content of WIDGET as seen in the display buffer."
  (let ((region (scoot-widget--region widget)))
    (buffer-substring-no-properties (car region)
                                    (cdr region))))


(defun scoot-widget--widget-start-column (widget)
  "Get the column number where WIDGET begins in the display buffer."
  (with-current-buffer (if (bound-and-true-p scoot-widget-display-buffer)
                           scoot-widget-display-buffer
                         (current-buffer))
    (save-excursion
      (goto-char (plist-get widget :widget-start))
      (current-column))))

(defun scoot-widget--shadow-buffer-content (widget)
  "Get the cursor/point position in shadow buffer of WIDGET."
  (with-scoot-widget-shadow-buffer widget
    (buffer-string)))



;; Shadow buffer / Widget positions

(defun scoot-widget--shadow-buffer-point (widget)
  "Get the cursor/point position in shadow buffer of WIDGET."
  (with-scoot-widget-shadow-buffer widget
    (point)))

(defun scoot-widget--point->point (widget &optional point opts)
  "Translate POINT to point within WIDGET.

OPTS may provide properties:
:from-absolute-p t/nil - A truthy value will make the function treat POINT
                         as absolute within the visible buffer, otherwise
                         relative from the widget start position.
:to-absolute-p t/nil - A truthy value will make the function return
                       an absolute point within the visible buffer, otherwise
                       relative from the widget start position.
:shadow-p t/nil - A truthy value will make the return value refer to
                  the position within the shadow buffer."
  (let* ((in-shadow-buf-p (buffer-live-p scoot-widget-display-buffer))
         (from-absolute-p (plist-get opts :from-absolute-p))
         (to-absolute-p (plist-get opts :to-absolute-p))
         (shadow-p (plist-get opts :shadow-p))
         (pos (scoot-widget--point->position widget point
                                             (list :shadow-p shadow-p
                                                   :absolute-p from-absolute-p))))
    (with-current-buffer (if (and shadow-p (not in-shadow-buf-p))
                             (scoot-widget--get-shadow-buffer :type (plist-get widget :type)
                                                              :name (plist-get widget :name))
                           (current-buffer))
      (cond
       (shadow-p (save-excursion
                   (goto-char (point-min))
                   (forward-line (1- (plist-get pos :line)))
                   (move-to-column (plist-get pos :col))
                   (point)))
       ((and in-shadow-buf-p) (with-current-buffer scoot-widget-display-buffer
                                (save-excursion
                                  (goto-char (plist-get widget :widget-start))
                                  (forward-line (1- (plist-get pos :line)))
                                  (move-to-column (+ (plist-get pos :col)
                                                     (scoot-widget--widget-start-column widget)))
                                  (if to-absolute-p
                                      (point)
                                    (1+ (- (point) (plist-get widget :widget-start)))))))
       ((and from-absolute-p (not to-absolute-p))
        (1+ (- point (plist-get widget :widget-start))))
       ((and to-absolute-p (not from-absolute-p))
        (1- (+ point (plist-get widget :widget-start))))
       (t point)))))

(defun scoot-widget--point->position (widget &optional point opts)
  "Calculate the position of POINT within WIDGET.

The term \"position\" refers to the plist format of:
\(:line <line number>
 :col <column number>)

OPTS may provide properties:
:absolute-p t/nil - A truthy value will make the function treat point and
                    position as absolute within the visible buffer,
                    otherwise relative from the widget start position.
:shadow-p t/nil - A truthy value will make the return value refer to
                  the position within the shadow buffer."
  (save-excursion
    (let ((absolute-p (plist-get opts :absolute-p))
          (shadow-p (plist-get opts :shadow-p))
          (is-shadow-buf-p (scoot-widget--is-shadow-buffer-p)))
      (goto-char (cond
                  ((and (not is-shadow-buf-p) (not absolute-p))
                   (1- (+ point (plist-get widget :widget-start))))
                  (t point)))
      (list :line (cond
                   ((and is-shadow-buf-p (not shadow-p) absolute-p)
                    (1- (+ (plist-get widget :widget-start-line) (line-number-at-pos))))
                   ((and (not is-shadow-buf-p) (or (not absolute-p) shadow-p))
                    (1+ (- (line-number-at-pos) (plist-get widget :widget-start-line))))
                   (t (line-number-at-pos)))
            :col (cond
                  ((and is-shadow-buf-p (not shadow-p) absolute-p)
                   (+ (current-column) (scoot-widget--widget-start-column widget)))
                  ((and (not is-shadow-buf-p) (or (not absolute-p) shadow-p))
                   (- (current-column) (scoot-widget--widget-start-column widget)))
                  (t (current-column)))))))



;; Widget Management Functions

(defun scoot-widget--make-identity (widget-type widget-name)
  "Combine WIDGET-TYPE and WIDGET-NAME to a unique identifier."
  (intern (format "%s--%s" widget-type widget-name)))

(cl-defun scoot-widget--register-widget! (widget-type widget-name)
  "Create and register widget entry in the current buffer.

The identity of the widget is created from WIDGET-TYPE and WIDGET-NAME."
  (let* ((identity (scoot-widget--make-identity widget-type widget-name))
         (widget-entry (cons identity (list :type widget-type
                                            :name widget-name))))
    (when (scoot-widget--get-widget :identity identity)
      (error (format "Buffer already contains widget with identity: '%s'" identity)))
    (push widget-entry scoot--active-widgets)
    (cdr widget-entry)))

(cl-defun scoot-widget--get-widget (&key type name identity)
  "Get an existing widget object in the current buffer.

The widget can be identified either by IDENTITY or a combination of TYPE
and NAME.

Examples:
    \(scoot-widget--get-widget :type `query-block :name `block-1)
    \(scoot-widget--get-widget :identity \"query-block--block-1\")"
  (let ((widget-identity (or identity
                             (scoot-widget--make-identity type name))))
    (alist-get widget-identity scoot--active-widgets)))

(defun scoot-widget--get-widget-config (widget-type widget-name)
  "Get the widget config for widget identified by WIDGET-TYPE and WIDGET-NAME."
  (let ((widget-identity (scoot-widget--make-identity widget-type
                                                      widget-name)))
    (if-let ((widget-config (scoot-widget--get-widget :identity widget-identity)))
        widget-config
      (scoot-widget--register-widget! widget-type widget-name))))

(make-obsolete 'scoot-widget--get-widget-config "Use scoot-widget--get-widget instead" nil)

(defun scoot-widget--destroy-widget! (widget)
  "Delete WIDGET from the current buffer."
  (when-let ((ov (plist-get widget :widget-overlay)))
    (delete-overlay ov))
  (when-let ((begin (plist-get widget :widget-start))
             (end (plist-get widget :widget-end)))
    (delete-region begin end)))

(defun scoot-widget--destroy-widgets! ()
  "Destroy all known Scoot widgets in the current buffer."
  (dolist (widget-entry scoot--active-widgets)
    (scoot-widget--destroy-widget! (cdr widget-entry)))
  (setq-local scoot--active-widgets nil))



;; Hooks

(defun scoot-widget--setup-input-mode (widget-type widget-name)
  "Setup all hooks for WIDGET-TYPE, WIDGET-NAME and override previous mode."
  (setq scoot-original-local-map (current-local-map))
  (use-local-map nil)

  (let ((widget (scoot-widget--get-widget :type widget-type
                                          :name widget-name)))
    (plist-put widget
               :pre-command-hook
               (lambda ()
                 (scoot-widget--pre-command-hook widget-type
                                                 widget-name)))

    (plist-put widget
               :post-command-hook
               (lambda ()
                 (scoot-widget--post-command-hook widget-type
                                                  widget-name)))

    (plist-put widget
               :after-change-hook
               (lambda (beg end len)
                 (scoot-widget--after-change-hook widget-type
                                                  widget-name
                                                  beg end len)))

    (add-hook 'pre-command-hook (plist-get widget :pre-command-hook) nil t)
    (add-hook 'post-command-hook (plist-get widget :post-command-hook) nil t)
    (add-hook 'after-change-function (plist-get widget :after-change-hook) nil t)))

(defun scoot-widget--teardown-input-mode (widget-type widget-name)
  "Disable all hooks for WIDGET-TYPE, WIDGET-NAME and restore previous mode."
  (use-local-map scoot-original-local-map)
  (setq scoot-original-local-map nil)

  (let ((widget (scoot-widget--get-widget :type widget-type
                                          :name widget-name)))
    (remove-hook 'pre-command-hook (plist-get widget :pre-command-hook) t)
    (remove-hook 'post-command-hook (plist-get widget :post-command-hook) t)
    (remove-hook 'after-change-function (plist-get widget :after-change-hook) t)

    (when (plist-get widget :remove-on-teardown)
      (setq-local scoot--active-widgets (assq-delete-all
                                         (scoot-widget--make-identity widget-type
                                                                      widget-name)
                                         scoot--active-widgets)))))

(defun scoot-widget--pre-command-hook (widget-type widget-name)
  "Prepare state for sync with shadow buffer.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget that a command is being performed on."
  (let ((widget (scoot-widget--get-widget :type widget-type
                                          :name widget-name)))
    (setq-local scoot--pre-command-point (point))
    (with-scoot-widget-shadow-buffer widget
      (setq-local scoot--pre-command-point (point)))
    (plist-put widget
               :command-advice
               (lambda (orig-fn &rest args)
                 (scoot-widget--command-args-advice widget-type
                                                    widget-name
                                                    orig-fn
                                                    args)))
    (advice-add this-command :around (plist-get widget :command-advice))))

(defun scoot-widget--post-command-hook (widget-type widget-name)
  "Check for cursor movement in the shadow buffer and replicate to the widget.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget that a command has been performed on."
  (condition-case err
      (let* ((widget (scoot-widget--get-widget :type widget-type
                                               :name widget-name))
             (contain-cursor (plist-get widget :contain-cursor))
             (sbuf-point (scoot-widget--shadow-buffer-point widget)))
        (when scoot--pre-command-point
          (when (or (/= sbuf-point (with-scoot-widget-shadow-buffer widget
                                     scoot--pre-command-point))
                    contain-cursor)
            (goto-char (with-scoot-widget-shadow-buffer widget
                         (scoot-widget--point->point widget
                                                     sbuf-point
                                                     (list :to-absolute-p t)))))))
    (progn (error
            (message "Error in scoot-widget--post-command-hook: %s - %s" (car err) (cdr err)))
           (signal (car err) (cdr err)))))

(defun scoot-widget--after-change-hook (_widget-type _widget-name _beg _end _len)
  "No operation for now.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget whose has changed.

BEG, END and LEN describe the change that has occured.")

(defun scoot-widget--command-args-advice (widget-type widget-name orig-fn args)
  "Advice around any command executing in the widget.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget whose has changed.  ORIG-FN is the function of the executing
command, with arguments in ARGS."
  (condition-case err
      (when-let* ((widget (scoot-widget--get-widget :type widget-type
                                                    :name widget-name))
                  (shadow-buffer (scoot-widget--get-shadow-buffer :widget widget)))
        (advice-remove this-command (plist-get widget :command-advice))
        (let* ((markp mark-active)
               (shadow-region-start (when markp (scoot-widget--point->point
                                                 widget (region-beginning)
                                                 '(:from-absolute-p t :shadow-p t))))
               (shadow-region-end (when markp (scoot-widget--point->point
                                               widget (region-end)
                                               '(:from-absolute-p t :shadow-p t))))
               (cmd (with-current-buffer shadow-buffer
                      (pcase this-command
                        (`left-char (list :shadow-p (not (bobp)) :exec 'backward-char))
                        (`right-char (list :shadow-p (not (eobp)) :exec 'forward-char))
                        (`forward-line (list :shadow-p t :exec (lambda ()
                                                                 (apply #'forward-line args))))
                        (`previous-line (list :shadow-p (not (= 1 (line-number-at-pos)))
                                              :exec (lambda () (forward-line -1))))
                        (`next-line (list :shadow-p (not (= (line-number-at-pos)
                                                            (line-number-at-pos (point-max))))
                                          :exec (lambda () (forward-line 1))))
                        (`move-end-of-line (list :shadow-p t :exec 'end-of-line))
                        (`move-beginning-of-line (list :shadow-p t :exec 'beginning-of-line))
                        (`left-word (list :shadow-p t :exec 'backward-word))
                        (`right-word (list :shadow-p t :exec 'forward-word))
                        (`self-insert-command (list :shadow-p t
                                                    :exec (lambda ()
                                                            (insert
                                                             (apply #'make-string args)))))
                        (`delete-backward-char (list :shadow-p (if (bobp) 'cancel t)
                                                     :exec (lambda () (delete-char -1))))
                        (`delete-forward-char (list :shadow-p (if (eobp) 'cancel t)
                                                    :exec (lambda () (delete-char 1))))
                        (`backward-kill-word (list :shadow-p (if (bobp) 'cancel t)
                                                   :exec (lambda () (backward-kill-word 1))))
                        (`kill-line (list :shadow-p t))
                        (`kill-word (list :shadow-p t))
                        (`kill-region (list :shadow-p t
                                            :exec (lambda () (funcall #'kill-region
                                                                      shadow-region-start
                                                                      shadow-region-end))
                                            :post 'deactivate-mark))
                        (`indent-for-tab-command (list :shadow-p t))
                        (`newline (list :shadow-p t))
                        (`undo (list :shadow-p t))
                        (`yank (list :shadow-p t :post 'deactivate-mark))))))
          (if cmd
              (progn
                (let ((shadow-p (plist-get cmd :shadow-p))
                      (exec (plist-get cmd :exec))
                      (exec-cmd (lambda (x)
                                  (cond
                                   ((null x) (apply orig-fn args))
                                   ((functionp x) (funcall x))
                                   ((symbolp x) (funcall (symbol-function x)))
                                   (t (message "Failed to call command action: %s %s"
                                               (type-of x) x)))))
                      (post (plist-get cmd :post))
                      (exec-post (lambda (x)
                                   (when (not (null x))
                                     (cond
                                      ((functionp x) (funcall x))
                                      ((symbolp x) (funcall (symbol-function x)))
                                      (t (message "Failed to call post-command action: %s %s"
                                                  (type-of x) x)))))))
                  (pcase shadow-p
                    (`t (with-current-buffer shadow-buffer
                          (funcall exec-cmd exec)))
                    (`nil (funcall exec-cmd exec)))
                  (funcall exec-post post)))
            (progn
              (apply orig-fn args)))))
    (progn
      (error
       (message "Error while executing command advice for %s: %s - %s" this-command (car err) (cdr err)))
      (signal (car err) (cdr err)))))


(defun scoot-widget--kill-shadow-buffer (shadow-buffer)
  "Kill the SHADOW-BUFFER.  Run as a hook when killing the parent buffer."
  (when (and shadow-buffer (buffer-live-p shadow-buffer))
    (kill-buffer shadow-buffer)))



;; Scoot Widget detection

(defun scoot-widget--cursor-sensor-fn (_win _pos _dir)
  "Cursor sensor function delegating to scoot-widget--detect-widget."
  (scoot-widget--detect-widget))

(defconst scoot-widget--cursor-sensor-functions (list #'scoot-widget--cursor-sensor-fn))

(declare-function scoot-qb--query-block-at-point-p "scoot-query-block")
(declare-function scoot-query-block-mode "scoot-query-block")
(declare-function scoot-table--table-at-point-p "scoot-query-table")
(declare-function scoot-table-mode "scoot-table")

 (defun scoot-widget--detect-widget ()
  "Check cursor position and handle query block activation/deactivation."
  (if (scoot-qb--query-block-at-point-p)
      (unless (bound-and-true-p scoot-query-block-mode) (scoot-query-block-mode 1))
    (when (bound-and-true-p scoot-query-block-mode) (scoot-query-block-mode -1)))

  (when (not (bound-and-true-p scoot-input-mode))
    (if (scoot-table--table-at-point-p)
        (unless (bound-and-true-p scoot-table-mode) (scoot-table-mode 1))
      (when (bound-and-true-p scoot-table-mode) (scoot-table-mode -1)))))


(provide 'scoot-widget)

;;; scoot-widget.el ends here
