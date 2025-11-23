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



;; Shadow buffer management methods

(cl-defgeneric scoot-widget--shadow-buffer-name (widget-type widget-name)
  "Construct the shadow buffer name for WIDGET-TYPE with WIDGET-NAME.")

(cl-defgeneric scoot-widget--get-shadow-buffer (widget-type widget-name)
  "Get the shadow buffer name for WIDGET-TYPE with WIDGET-NAME.")

(cl-defgeneric scoot-widget--set-shadow-buffer (widget-type widget-name buffer)
  "Set the shadow buffer name for WIDGET-TYPE with WIDGET-NAME to BUFFER.")

(cl-defgeneric scoot-widget--shadow-after-change-hook (widget-type widget-name)
  "Return a reference to function to  run when the shadow buffer changes.

The method delegates on WIDGET-TYPE an may optionally use WIDGET-NAME to
determine the update method.")



;; Shadow buffer control functions

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
        (shadow-buffer (scoot-widget--get-shadow-buffer widget-type widget-name)))
    (unless (and shadow-buffer
                 (buffer-live-p shadow-buffer))

      (setq shadow-buffer (get-buffer-create
                           (scoot-widget--shadow-buffer-name widget-type
                                                             widget-name)))
      (scoot-widget--set-shadow-buffer widget-type
                                       widget-name
                                       shadow-buffer)

      (with-current-buffer shadow-buffer
        (setq-local scoot-widget-display-buffer parent-buf)
        (setq-local tab-width (buffer-local-value 'tab-width parent-buf))
        (setq-local indent-tabs-mode (buffer-local-value 'indent-tabs-mode parent-buf))

        (add-hook 'after-change-functions (lambda (beg end len)
                                            (funcall (scoot-widget--shadow-after-change-hook
                                                      widget-type widget-name)
                                                     beg end len))
                  nil t))
      (add-hook 'kill-buffer-hook (lambda ()
                                    (scoot-widget--kill-shadow-buffer
                                     (scoot-widget--get-shadow-buffer widget-type
                                                                      widget-name)))
                nil t))

    (scoot-widget--reset-shadow-buffer shadow-buffer content)
    (scoot-widget--get-shadow-buffer widget-type widget-name)))

(defun scoot-widget--reset-shadow-buffer (shadow-buffer content)
  "Clear and reset the SHADOW-BUFFER contents.

CONTENT should be a single string formatted with \"\\n\" as line separators."
  (with-current-buffer shadow-buffer
    (scoot--save-cursor)
    (with-silent-modifications
      (erase-buffer)
      (insert content))
    (scoot--restore-cursor)))


;; Shadow buffer / Widget positions

(defun scoot-widget--translate-point (widget point &optional shadow-buffer)
  "Translate POINT to its corresponding point in the WIDGET mirror buffer.

If called inside the visible buffer, the point is translated to its
corresponding point in the shadow buffer.

If called inside the shadow buffer - with SHADOW-BUFFER supplied -
the point is translated to its corresponding point in the visible buffer."
  (let* ((shadow-buf-p (buffer-live-p scoot-widget-display-buffer))
         (buffer-pos (scoot-widget--get-widget-pos point (not shadow-buf-p)))
         (translated-pos (scoot-widget--translate-pos widget buffer-pos)))
    (if shadow-buf-p
        (with-current-buffer scoot-widget-display-buffer
          (scoot--pos-to-point translated-pos))
      (with-current-buffer shadow-buffer
        (scoot--pos-to-point translated-pos)))))

(defun scoot-widget--point->position (widget &optional point opts)
  "Calculate the position of POINT within WIDGET.

The term \"position\" refers to the plist format of:
\(:line <line number>
 :col <column number>)

OPTS may provide properties:
:absolute-p t/nil - A truthy value will make the function treat point and
                  position treated as absolute within the visible buffer,
                  otherwise relative from the widget start position.
:shadow-p t/nil - A truthy value will make the return value refer to
                  the position within the shadow buffer."
  (save-excursion
    (let ((absolute-p (plist-get opts :absolute-p))
          (shadow-p (plist-get opts :shadow-p))
          (is-shadow-buf-p (scoot-widget--is-shadow-buffer-p)))
      (if is-shadow-buf-p
          (progn
            (goto-char point)
            (list :line (if (and (not shadow-p) absolute-p)
                            (1- (+ (plist-get widget :widget-start-line) (line-number-at-pos)))
                          (line-number-at-pos))
                  :col (current-column)))
        (progn
          (goto-char (if absolute-p
                         point
                       (1- (+ point (plist-get widget :widget-start)))))
          (list :line (if (or (not absolute-p) shadow-p)
                          (1+ (- (line-number-at-pos) (plist-get widget :widget-start-line)))
                        (line-number-at-pos))
                :col (current-column)))))))

(defun scoot-widget--get-widget-pos (widget &optional point shadow-buf-p)
  "Calculate the cursor position of POINT within the editable WIDGET.

The term \"position\" refers to the plist format of:
\(:line <line number>
 :col <column number>)

Defaults to calculating the relative position in the visible widget.
Passing non-nil value for SHADOW-BUF-P instead returns the absolute position
in the shadow-buffer."
  (let ((pt (or point (point))))
    (list :line (if shadow-buf-p
                    (line-number-at-pos pt)
                  (- (line-number-at-pos pt) (1- (line-number-at-pos (marker-position (plist-get widget :editable-start))))))
          :col (current-column))))

(defun scoot-widget--translate-pos (widget pos)
  "Translate POS to its corresponding pos in the WIDGET mirror buffer.

If called inside the visible buffer, the position is translated to its
corresponding position in the shadow buffer.

If called inside the shadow buffer, the position is translated to its
corresponding position in the visible buffer."
  (let ((qb-line-offset (with-current-buffer
                            (or scoot-widget-display-buffer
                                (current-buffer))
                          (line-number-at-pos (marker-position (plist-get widget :editable-start))))))
    (if (buffer-live-p scoot-widget-display-buffer)
        (with-current-buffer scoot-widget-display-buffer
          (list :line (+ (plist-get pos :line) (1- qb-line-offset))
                :col (plist-get pos :col)))
      (list :line (- (plist-get pos :line) (1- qb-line-offset))
            :col (plist-get pos :col)))))


;; Widget Config Functions

(defun scoot-widget--make-identity (widget-type widget-name)
  "Combine WIDGET-TYPE and WIDGET-NAME to a unique identifier."
  (intern (format "%s--%s" widget-type widget-name)))

(defun scoot-widget--get-widget-config (widget-type widget-name)
  "Get the widget config for widget identified by WIDGET-TYPE and WIDGET-NAME."
  (let ((widget-identity (scoot-widget--make-identity widget-type
                                                      widget-name)))
    (if-let ((widget-config (alist-get widget-identity
                                       scoot--active-widgets)))
        widget-config
      (progn (push (cons widget-identity (list :type widget-type
                                               :name widget-name))
                   scoot--active-widgets)
             (alist-get widget-identity
                        scoot--active-widgets)))))



;; Hooks

(defun scoot-widget--setup-input-mode (widget-type widget-name)
  "Setup all hooks for WIDGET-TYPE, WIDGET-NAME and override previous mode."
  (setq scoot-original-local-map (current-local-map))
  (use-local-map nil)

  (let ((widget (scoot-widget--get-widget-config widget-type
                                                 widget-name)))
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
                 (scoot-widget--after-change-hook beg end len
                                                  widget-type
                                                  widget-name)))

    (add-hook 'pre-command-hook (plist-get widget :pre-command-hook) nil t)
    (add-hook 'post-command-hook (plist-get widget :post-command-hook) nil t)
    (add-hook 'after-change-function (plist-get widget :after-change-hook) nil t)))

(defun scoot-widget--teardown-input-mode (widget-type widget-name)
  "Disable all hooks for WIDGET-TYPE, WIDGET-NAME and restore previous mode."
  (use-local-map scoot-original-local-map)
  (setq scoot-original-local-map nil)

  (let ((widget (scoot-widget--get-widget-config widget-type widget-name)))
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
  (let ((widget (scoot-widget--get-widget-config widget-type widget-name)))
    (setq-local scoot--pre-command-point (point))
    (with-current-buffer (scoot-widget--get-shadow-buffer widget-type widget-name)
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
      (let* ((widget (alist-get (scoot-widget--make-identity widget-type
                                                             widget-name)
                                scoot--active-widgets))
             (shadow-buffer (scoot-widget--get-shadow-buffer widget-type
                                                             widget-name))
             (contain-cursor (plist-get widget :contain-cursor))
             (sbuf-pos (with-current-buffer shadow-buffer
                         (list :line (line-number-at-pos)
                               :col (current-column)))))
        (when scoot--pre-command-point
          (when (or (with-current-buffer shadow-buffer
                      (/= (point) scoot--pre-command-point))
                    contain-cursor)
            (goto-char (plist-get widget :editable-start))
            (when (> (plist-get sbuf-pos :line) 1)
              (forward-line (1- (plist-get sbuf-pos :line))))
            (forward-char (plist-get sbuf-pos :col)))))
    (error
     (message "Error in scoot-widget--post-command-hook: %s - %s" (car err) (cdr err)))))

(defun scoot-widget--after-change-hook (_widget-type _widget-name _beg _end _len)
  "No operation for now.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget whose has changed.

BEG, END and LEN describe the change that has occured."
  nil)

(defun scoot-widget--command-args-advice (widget-type widget-name orig-fn args)
  "Advice around any command executing in the widget.

WIDGET-TYPE and WIDGET-NAME are used to identify the config of the
widget whose has changed.
ORIG-FN is the function of the executing command, with arguments in ARGS."
  (condition-case err
      (let ((widget (scoot-widget--get-widget-config widget-type
                                                     widget-name))
            (shadow-buffer (scoot-widget--get-shadow-buffer widget-type
                                                            widget-name)))
        (advice-remove this-command (plist-get widget :command-advice))
        (let* ((markp mark-active)
               (region-start (when markp (scoot-widget--translate-point widget (region-beginning))))
               (region-end (when markp (scoot-widget--translate-point widget (region-end))))
               (cmd (with-current-buffer shadow-buffer
                      (pcase this-command
                        (`left-char (list :shadow-p (not (bobp)) :exec 'backward-char))
                        (`right-char (list :shadow-p (not (eobp)) :exec 'forward-char))
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
                                                                      region-start
                                                                      region-end))
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
    (error
     (message "Error while executing command advice for %s: %s - %s" this-command (car err) (cdr err)))))

(defun scoot-widget--kill-shadow-buffer (shadow-buffer)
  "Kill the SHADOW-BUFFER.  Run as a hook when killing the parent buffer."
  (when (and shadow-buffer (buffer-live-p shadow-buffer))
    (kill-buffer shadow-buffer)))

(provide 'scoot-widget)

;;; scoot-widget.el ends here
