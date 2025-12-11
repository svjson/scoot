;;; scoot-test-buffer-fixtures.el --- test utilities/fixtures for scoot-table -*- lexical-binding: t -*-

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

;; This file contains fixtures and high-level functions for automating table
;; interaction during tests.

;;; Code:

(require 'scoot-test-fixtures)
(require 'scoot-input)
(require 'scoot-table)



;; General buffer movement

(defun scoot-test-buffer--move-to-coord (coord)
  "Move point COORD in the current buffer.

COORD expects the format of (<line> . <row>)."
  (goto-char (point-min))
  (forward-line (1- (car coord)))
  (move-to-column (cdr coord)))



;; Buffer management

(defmacro with-new-window-buffer (&rest body)
  "Macro that executes BODY within a \"real\" temporary buffer.

Unlike `with-temp-buffer` this buffer attaches to the window, allowing
commands and hooks to run properly."
  `(let ((buf (generate-new-buffer "*scoot test-buffer*")))
     (unwind-protect
         (progn
           (switch-to-buffer buf)
           ,@body)
       (kill-buffer buf))))



;; Buffer state & debug

(defun scoot-active-minor-modes ()
  "Round up active minor modes relevant to scoot and scoot tests."
  (cl-remove-if-not
   (lambda (mode)
	   (memq mode '(cursor-sensor-mode
                  read-only-mode
                  scoot-query-block-mode
                  scoot-input-mode
                  scoot-table-mode)))
   local-minor-modes))

(defun scoot-command-hooks ()
  "Get the active scoot-related command hooks."
  (cl-flet ((filter-hooks (hook-list white-list)
              (mapcar
               (lambda (h)
                 (let ((hstr (format "%S" h)))
                   (if (interpreted-function-p h)
                       (let* ((spos (string-search "scoot" hstr)))
                         (intern (substring hstr spos (string-search " " hstr spos))))
                     h)))
               (seq-filter (lambda (h)
                             (or (memq h white-list)
                                 (string-search "scoot" (format "%S" h))))
                           hook-list))))
    (list :pre-command-hook (filter-hooks pre-command-hook '())
          :post-command-hook (filter-hooks post-command-hook '(cursor-sensor--detect)))))

(defun scoot-buffer-widgets ()
  "Round up widgets present in the current buffer."
  (cl-flet ((widget-edge (w prop)
              (let ((pos (plist-get (cdr w) prop)))
                (list :point (marker-position  pos)
                      :pos (when pos (cons (line-number-at-pos pos)
                                           (save-excursion
                                             (goto-char pos)
                                             (current-column))))))))
    (mapcar (lambda (w)
              (list :id (car w)
                    :from (widget-edge w :widget-start)
                    :to (widget-edge w :widget-end)))
            scoot--active-widgets)))

(defun scoot-buffer-state ()
  "Round up relevant scoot buffer state."
  (append
   (list :major-mode major-mode
         :minor-modes (scoot-active-minor-modes)
         :widgets (scoot-buffer-widgets))
   (scoot-command-hooks)))




(provide 'scoot-test-buffer-fixtures)


;;; scoot-test-buffer-fixtures.el ends here
