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
;; SQL code, as used by scoot-result-modeg

;;; Code:

(require 'simple)
(require 'scoot-common)

(defvar-local scoot-original-local-map nil
  "Backup of the original local map before enabling query edit mode.")

(defvar-local scoot-query-block-start nil
  "The start position of the query block in the current buffer.")

(defvar-local scoot-query-block-end nil
  "The end position of the query block in the current buffer.")

(defvar-local scoot-query-block-content nil
  "Snapshot of the current contents of the query-block.")

(defvar-local scoot-query-block-shadow-buffer nil
  "Reference to the shadow buffer backing the query block.")

(defvar-local scoot-query-block-display-buffer nil
  "Reference from the shadow buffer to the main display buffer.")

(defvar-local scoot--pre-command-point nil)

(defface scoot-query-block-face
  '((t :inherit highlight :background "#222222" :extend nil))
  "Face used fo query blocks."
  :group 'scoot)

(defface scoot-query-block-padding-face
  '((t :inherit scoot-query-block-face :extend nil))
  "Face used fo query blocks."
  :group 'scoot)

(declare-function scoot-result--execute-query "scoot-result")

(defun scoot-qb--query-block-at-p (pt)
  "Return non-nil if point PT is inside the query block."
  (eq (alist-get 'thing (scoot--props-at pt)) 'query-block))

(defun scoot-qb--query-block-at-point-p ()
  "Return non-nil if the cursor is inside the query block."
  (scoot-qb--query-block-at-p (point)))

(defun scoot-qb--init-shadow-buffer (query)
  "Initialize the shadow buffer containing the actual editable query.

QUERY should be a single string formatted with \"\\n\" as line separators.

The resulting buffer is an invisible buffer containing the editable query,
and is used to execute editing and movement commands without the need to
take the padding and property rules of the display buffer into consideration.

The buffer is assigned to the buffer-local variable
`scoot-query-block-shadow-buffer` of the buffer from which this function
was invoked, which should typically be `scoot-result-mode` buffer."
  (let ((parent-buf (current-buffer)))
    (unless (and scoot-query-block-shadow-buffer
                 (buffer-live-p scoot-query-block-shadow-buffer))
      (setq-local scoot-query-block-shadow-buffer
                  (get-buffer-create (format " *scoot shadow-qb(%s)*"
                                             (buffer-name))))
      (with-current-buffer scoot-query-block-shadow-buffer
        (setq-local scoot-query-block-display-buffer parent-buf)
        (add-hook 'after-change-functions 'scoot-qb--shadow-after-change-hook nil t))
      (add-hook 'kill-buffer-hook #'scoot-qb--kill-shadow-buffer nil t)))
  (scoot-qb--reset-shadow-buffer query)
  scoot-query-block-shadow-buffer)

(defun scoot-qb--reset-shadow-buffer (query)
  "Clear and reset the shadow buffer contents.

QUERY should be a single string formatted with \"\\n\" as line separators."
  (with-current-buffer scoot-query-block-shadow-buffer
    (scoot--save-cursor)
    (with-silent-modifications
      (erase-buffer)
      (insert query))
    (scoot--restore-cursor)))

(defun scoot-qb--kill-shadow-buffer ()
  "Kill the shadow buffer.  Run as a hook when killing the parent buffer."
  (when (buffer-live-p scoot-query-block-shadow-buffer)
    (kill-buffer scoot-query-block-shadow-buffer)))

(defun scoot-qb--build-query-block (query)
  "Build and propertize query block contents from QUERY into a string."
  (let* ((width (window-body-width))
         (wrapped-lines (scoot--wrap-string query width))
         (content ""))
    (dolist (line wrapped-lines)
      (setq content (concat content
                            (propertize line
                                        'face 'scoot-query-block-face
                                        'thing 'query-block)
                            (propertize (make-string
                                         (1- (- width (length line)))
                                         ?\s)
                                        'face 'scoot-query-block-padding-face
                                        'thing 'query-block
                                        'qb-padding t)
                            "\n")))
    content))

(defun scoot-qb--insert-query-block (query)
  "Insert QUERY string with wrapping and box-like styling.

This should be called when initially setting up a query block.
For subsequent updates/refreshes of the query block, call
`scoot-qb--refresh-query-block`."
  (let* ((width (window-body-width))
         (wrapped-lines (scoot--wrap-string query width)))
    (setq-local scoot-query-block-start (point))
    (scoot-qb--init-shadow-buffer (string-join wrapped-lines "\n"))
    (insert (scoot-qb--build-query-block (scoot-qb--get-query)))
    (setq-local scoot-query-block-end (point))))

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
      (setq-local scoot-query-block-end (point))))
  (scoot--restore-cursor))

(defun scoot-qb--enter-query-block ()
  "Synchronize cursor positions when entering the visible query block."
  (let ((qb-pos (scoot-qb--get-qb-pos (point)))
        (updated-pos nil))
    (with-current-buffer scoot-query-block-shadow-buffer
      (scoot-qb--move-to-qb-pos qb-pos)
      (setq updated-pos (list :line (line-number-at-pos) :col (current-column))))
    (scoot-qb--move-to-qb-pos updated-pos scoot-query-block-start)))

(defun scoot-qb--translate-pos (pos)
  "Translate POS to its corresponding pos in the mirror buffer.

If called inside the visible buffer, the position is translated to its
corresponding position in the shadow buffer.

If called inside the shadow buffer, the position is translated to its
corresponding position in the visible buffer."
  (let ((qb-line-offset (with-current-buffer
                            (or scoot-query-block-display-buffer
                                (current-buffer))
                          (line-number-at-pos scoot-query-block-start))))
    (if (buffer-live-p scoot-query-block-display-buffer)
        (with-current-buffer scoot-query-block-display-buffer
          (list :line (+ (plist-get pos :line) (1- qb-line-offset))
                :col (plist-get pos :col)))
      (list :line (- (plist-get pos :line) (1- qb-line-offset))
            :col (plist-get pos :col)))))

(defun scoot-qb--translate-point (point)
  "Translate POINT to its corresponding point in the mirror buffer.

If called inside the visible buffer, the point is translated to its
corresponding point in the shadow buffer.

If called inside the shadow buffer, the point is translated to its
corresponding point in the visible buffer."
  (let* ((shadow-buf-p (buffer-live-p scoot-query-block-display-buffer))
         (buffer-pos (scoot-qb--get-qb-pos point (not shadow-buf-p)))
         (translated-pos (scoot-qb--translate-pos buffer-pos)))
    (if shadow-buf-p
        (with-current-buffer scoot-query-block-display-buffer
          (scoot--pos-to-point translated-pos))
      (with-current-buffer scoot-query-block-shadow-buffer
        (scoot--pos-to-point translated-pos)))))

(defun scoot-qb--get-qb-pos (&optional point shadow-buf-p)
  "Calculate the cursor position of POINT within the editable query.

The term \"position\" refers to the plist format of:
\(:line <line number>
 :col <column number>)

Defaults to calculating the relative position in the visible query block.
Passing non-nil value for SHADOW-BUF-P instead returns the absolute position
in the shadow-buffer."
  (let ((pt (or point (point))))
    (list :line (if shadow-buf-p
                    (line-number-at-pos pt)
                  (- (line-number-at-pos pt) (1- (line-number-at-pos scoot-query-block-start))))
          :col (current-column))))

(defun scoot-qb--move-to-qb-pos (pos &optional initial-pos)
  "Move the cursor to POS inside the query block, if possible.

INITIAL-POS allows overriding the default of (point-min)."
  (goto-char (or initial-pos (point-min)))
  (forward-line (1- (plist-get pos :line)))
  (forward-char (min (plist-get pos :col) (- (line-end-position) (line-beginning-position)))))

(defun scoot-qb--get-query ()
  "Return the query currently entered into the query block."
  (with-current-buffer scoot-query-block-shadow-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun scoot-qb--shadow-after-change-hook (_ _ _)
  "Run after modification happens in shadow buffer.

BEG, END and LEN detail the beginning, end and length of the change."
  (with-current-buffer scoot-query-block-display-buffer
    (scoot-qb--refresh-query-block)))

(defun scoot-qb--post-command-hook ()
  "Check for cursor movement in the shadow buffer and replicate to query block."
  (let ((sbuf-pos (with-current-buffer scoot-query-block-shadow-buffer
                    (list :line (line-number-at-pos)
                          :col (current-column)))))
    (when (with-current-buffer scoot-query-block-shadow-buffer
            (/= (point) scoot--pre-command-point))
      (goto-char (point-min))
      (forward-line (+ (plist-get sbuf-pos :line)
                       (- (line-number-at-pos scoot-query-block-start) 2)))
      (forward-char (plist-get sbuf-pos :col)))))

(defun scoot-qb--pre-command-hook ()
  "Prepare state for sync with shadow buffer."
  (setq-local scoot-pre-command-point (point))
  (with-current-buffer scoot-query-block-shadow-buffer
    (setq-local scoot--pre-command-point (point)))
  (advice-add this-command :around #'scoot-qb--command-args-advice))

(defun scoot-qb--command-args-advice (orig-fn &rest args)
  "Advice around any command executing in the query block.

ORIG-FN is the function of the executing command, with arguments in ARGS."
  (advice-remove this-command #'scoot-qb--command-args-advice)
  (let* ((markp mark-active)
         (region-start (when markp (scoot-qb--translate-point (region-beginning))))
         (region-end (when markp (scoot-qb--translate-point (region-end))))
         (cmd (with-current-buffer scoot-query-block-shadow-buffer
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
              (`t (with-current-buffer scoot-query-block-shadow-buffer
                    (funcall exec-cmd exec)))
              (`nil (funcall exec-cmd exec)))
            (funcall exec-post post)))
      (progn
        (apply orig-fn args)))))

(defvar scoot-query-block-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") #'scoot-result--execute-query)
    map)
  "Keymap for `scoot-query-block-mode'.")

(define-minor-mode scoot-query-block-mode
  "Minor mode for editing and managing the query-block in Scoot Result Mode."
  :lighter " QueryBlock"
  :keymap scoot-query-block-mode-map

  (if scoot-query-block-mode
      (progn
        (setq scoot-original-local-map (current-local-map))
        (use-local-map nil)
        (read-only-mode -1)
        (add-hook 'pre-command-hook 'scoot-qb--pre-command-hook nil t)
        (add-hook 'post-command-hook 'scoot-qb--post-command-hook nil t)
        (add-hook 'after-change-functions 'scoot-qb--after-change-hook nil t)
        (scoot-qb--enter-query-block))
    (progn
      (use-local-map scoot-original-local-map)
      (setq scoot-original-local-map nil)
      (read-only-mode 1)
      (remove-hook 'pre-command-hook 'scoot-qb--pre-command-hook t)
      (remove-hook 'post-command-hook 'scoot-qb--post-command-hook t)
      (remove-hook 'after-change-functions 'scoot-qb--after-change-hook t))))


(provide 'scoot-query-block)

;;; scoot-query-block.el ends here

