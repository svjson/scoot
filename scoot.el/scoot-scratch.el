;;; scoot-scratch.el --- summary -*- lexical-binding: t -*-

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

;; Implements support for Scoot scratch buffers — interactive, editable
;; buffers designed for composing and executing raw SQL queries.
;;
;; A scratch buffer provides a lightweight SQL editing environment that can
;; be connected to a configured database connection.  Users can send SQL queries
;; directly to the `scoot-server`, view results in dedicated result buffers,
;; and optionally persist or reload their scratch contents.
;;
;; The buffer can be associated with a connection either manually or via
;; project-local or per-buffer configuration, and includes commands for
;; executing the current SQL statement (e.g., the one before point or in a
;; selected region), refreshing results, and managing autosave behavior.
;;
;; See also `scoot-result.el` for how result sets are rendered, and
;; `scoot.el` for overall Scoot integration.

;;; Code:

(require 'treesit)
(require 'sql)
(require 'scoot-connection)
(require 'scoot-result)

(defcustom scoot-scratch-directory (expand-file-name "~/.scoot/scratches/")
  "Directory to store Scoot scratch files."
  :type 'directory
  :group 'scoot)

(defcustom scoot-auto-enable-scratch-mode t
  "If non-nil, automatically enable `scoot-scratch-mode` for Scoot scratch files."
  :type 'boolean
  :group 'scoot)

(defun scoot-ensure-scratch-directory ()
  "Ensure that the configured scratch directory exists."
  (unless (file-directory-p scoot-scratch-directory)
    (make-directory scoot-scratch-directory t)))

(defun scoot-scratch--list-scratch-files ()
  "Return a list of scratch files (with full paths) in `scoot-scratch-directory`."
  (when (file-directory-p scoot-scratch-directory)
    (directory-files scoot-scratch-directory t "\\.scoot\\'")))

;;;###autoload
(defun scoot-new-scratch ()
  "Create a new Scoot scratch buffer."
  (interactive)
  (scoot-ensure-scratch-directory)
  (when-let* ((conn-name (scoot-connection--connection-prompt))
              (conn-string (or (gethash conn-name scoot-connections)
                               (read-string (format "Connection string for '%s': " conn-name)))))
    (unless (gethash conn-name scoot-connections)
      (scoot-add-connection conn-name conn-string))
    (when-let* ((scratch-name (read-string (format "Scratch name: ") nil nil conn-name))
                (filename (expand-file-name (concat scratch-name ".scoot") scoot-scratch-directory))
                (buffer (find-file-noselect filename)))
      (with-current-buffer buffer
        (erase-buffer)
        (when conn-string
          (insert (format "-- @connection-string: %s\n" conn-string)))
        (when conn-name
          (insert (format "-- @connection-name: %s\n\n" conn-name)))
        (scoot-scratch-mode)
        (setq-local scoot-connection-name conn-name))
      (pop-to-buffer buffer))))

;;;###autoload
(defun scoot-open-scratch ()
  "Prompt to open a Scoot scratch file from `scoot-scratch-directory`."
  (interactive)
  (let* ((files (scoot-scratch--list-scratch-files))
         (choices (mapcar #'file-name-nondirectory files))
         (selection (completing-read "Open scratch: " choices))
         (full-path (expand-file-name selection scoot-scratch-directory)))
    (find-file full-path)))

(defun scoot--preceding-delimiter-semicolon ()
  "Search backwards for a semicolon delimiter.

Does not consider semi-colons as part of strings.
TODO: Also consider other non-delimiter variants

Returns point or nil."
  (interactive)
  (let ((found nil))
    (save-excursion
      (let ((p (point)))
        (while (and (not found)
                    (re-search-backward ";" nil t))
          (unless (nth 3 (syntax-ppss)) ;; nth 3 = inside string
            (when (> (length (string-trim (buffer-substring (1+ (point)) p))) 0)
              (setq found (point)))))))
    (or found 1)))

(defun scoot-split-sql-statements (text)
  "Naively split TEXT into SQL statements using semicolons."
  (split-string text ";[ \n]*" t))

(defun scoot-statement-before-point ()
  "Return the SQL statement ending at point."
  (let ((statement (buffer-substring (scoot--preceding-delimiter-semicolon) (point))))
    (if (string-prefix-p ";" statement)
        (string-trim (substring statement 1))
      (string-trim statement))))

(defun scoot--parse-context-annotation (line)
  "Parse property name and value from context annotation from LINE."
  (when (string-match
         "^[\s]*-[-]+[\s]*@\\([a-zA-Z-]+\\):\\(.*\\)"
         line)
    (cons (match-string 1 line)
          (string-trim (match-string 2 line)))))

(defun scoot-scratch--resolve-context-at-point ()
  "Extracts relevant SQL lines and connection context."
  (save-excursion
    (let ((lines '())
          (stop nil)
          (context-props (make-hash-table :test 'equal)))
      (while (not stop)
        (when (eq (line-number-at-pos) 1)
          (setq stop t))
        (let* ((eol (point))
               (line (buffer-substring-no-properties (line-beginning-position) eol)))
          (if (string-match-p "^[\s]*-[-]+" line)
              (when-let* ((prop (scoot--parse-context-annotation line))
                          (prop-name (car prop)))
                (if (gethash prop-name context-props)
                    (setq stop t)
                  (puthash prop-name (cdr prop) context-props)))
            (if (eq 0 (hash-table-count context-props))
                (push line lines)
              (setq stop t)))
          (forward-line -1)
          (end-of-line)))
      (cons (string-join lines "\n")
            context-props))))

(cl-defun scoot-scratch--context-resolve-connection-name (&allow-other-keys)
  "Resolve the connection-name from the annotated context at point."
  (let* ((ctx (scoot-scratch--resolve-context-at-point))
         (ctx-props (cdr ctx)))
    (gethash "connection-name" ctx-props)))

(defun scoot-scratch--ensure-ctx-connection (ctx-props callback)
  "Ensure that the connection specified in the scratch context CTX-PROPS is valid.

The verified connection will be passed as an argument to CALLBACK to allow
execution to resume."
  (let ((connection-name (gethash "connection-name" ctx-props))
        (connection-string (gethash "connection-string" ctx-props)))
    (cond
     ((and connection-name connection-string)
      (scoot-connection--ensure-connection-by-name-and-string
       connection-name
       connection-string
       callback))

     ((and connection-name (gethash connection-name scoot-connections))
      (funcall callback (gethash connection-name scoot-connections)))

     (connection-string
      (let ((match nil))
        (maphash (lambda (_ conn)
                   (when (and (null match)
                              (equal (plist-get conn :url) connection-string))
                     (setq match conn)))
                 scoot-connections)
        (funcall callback match))))))

(defun scoot-scratch--ctx-operation-at-point (callback)
  "Prepare the stage for an operation depending on scratch context.

Resolves the current context and ensures the configured connection,
and then calls CALLBACK with the resolved information."
  (let* ((ctx (scoot-scratch--resolve-context-at-point))
         (ctx-content (car ctx))
         (ctx-props (cdr ctx)))
    (scoot-scratch--ensure-ctx-connection
     ctx-props
     (lambda (connection)
       (funcall callback
                connection
                ctx
                ctx-content
                ctx-props)))))

(defun scoot-eval-statement-before-point ()
  "Evaluate SQL statement before point."
  (interactive)
  (scoot-scratch--ctx-operation-at-point
   (lambda (connection _ ctx-content &rest _)
     (with-temp-buffer
       (insert ctx-content)
       (goto-char (point-max))
       (scoot-connection--execute-statement
        connection
        (scoot-statement-before-point)
        #'scoot-result--open-result-buffer)))))

(defun scoot-eval-region (beg end)
  "Evaluate all SQL statements in the selected region.

BEG and END describe the region start and end"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (stmts (scoot-split-sql-statements region)))
    (scoot-scratch--ctx-operation-at-point
     (lambda (connection &rest _)
       (dolist (stmt stmts)
         (scoot-connection--execute-statement
          connection
          stmt
          #'scoot-result--open-result-buffer))))))

(defface scoot-scratch-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for general SQL-style comments in scoot scratch buffers."
  :group 'scoot)

(defface scoot-scratch-annotation-key-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the @key part in scoot annotations."
  :group 'scoot)

(defface scoot-scratch-annotation-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the variable name in scoot annotations."
  :group 'scoot)

(defface scoot-scratch-annotation-value-face
  '((t :inherit font-lock-string-face))
  "Face for the value in scoot annotations."
  :group 'scoot)

(declare-function scoot-describe-table "scoot")
(declare-function scoot-list-databases "scoot")
(declare-function scoot-list-schemas "scoot")
(declare-function scoot-list-tables "scoot")

(defun scoot-scratch--match-comment-or-annotation (limit)
  "Match Scoot annotations or comments up to LIMIT.

Used to extend the basic `sql-mode` font-locking to handle
scratch comments and configuration annotations."
  (while (re-search-forward "^\\s-*--\\s-*\\(.*\\)$" limit t)
    (let* ((content (match-string 1))
           (start (match-beginning 1))
           (end (match-end 1)))
      (cond
       ((string-match "^\\(@\\)\\([a-zA-Z0-9-]+\\)\\(:\\)\\(.*\\)$" content)
        (let ((_ (match-beginning 0)))
          (add-text-properties (+ start (match-beginning 1))
                               (+ start (match-end 1))
                               '(face scoot-scratch-annotation-key-face))
          (add-text-properties (+ start (match-beginning 2))
                               (+ start (match-end 2))
                               '(face scoot-scratch-annotation-name-face))
          (add-text-properties (+ start (match-beginning 4))
                               (+ start (match-end 4))
                               '(face scoot-scratch-annotation-value-face))))
       (t
        (add-text-properties start end '(face scoot-scratch-comment-face)))))
    t))

(defun scoot-scratch--enable-font-lock ()
  "Enable Scoot-specific font-lock rules in current buffer."
  (font-lock-add-keywords nil
                          `((scoot-scratch--match-comment-or-annotation))
                          'append)
  (font-lock-flush)
  (font-lock-ensure))

(defun scoot--maybe-enable-scratch-mode ()
  "Auto-enable `scoot-scratch-mode` for scratch files, respecting user preference."
  (when scoot-auto-enable-scratch-mode
    (scoot-scratch-mode)))

(defvar scoot-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'scoot-eval-statement-before-point)
    (define-key map (kbd "C-c C-r") #'scoot-eval-region)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    map)
  "Keymap for `scoot-scratch-mode'.")

;;;###autoload
(define-derived-mode scoot-scratch-mode prog-mode "Scoot Scratch"
  "Major mode for Scoot SQL scratch buffers."
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local scoot-connection-name nil)

  (setq-local scoot-local--connection-name-resolvers
              '(scoot-scratch--context-resolve-connection-name
                scoot-connection--connection-prompt))

  (setq-local scoot-local--table-name-resolvers
              '(scoot--object-name-in-region
                scoot--object-name-at-point
                scoot-connection--table-prompt))

  (when (not (when (treesit-available-p)
               (when (treesit-language-available-p 'sql)
                 (treesit-parser-create 'sql)
                 (setq-local major-mode-remap-alist '((sql-mode . scoot-scratch-mode)))
                 (treesit-major-mode-setup)
                 t)))
    (setq-local font-lock-defaults
                '(sql-mode-ms-font-lock-keywords nil t)))
  (scoot-scratch--enable-font-lock)
  (scoot-ensure-server))

(unless (assoc "\\.scoot\\'" auto-mode-alist)
  (add-to-list 'auto-mode-alist
               '("\\.scoot\\'" . scoot--maybe-enable-scratch-mode)))

(provide 'scoot-scratch)

;;; scoot-scratch.el ends here
