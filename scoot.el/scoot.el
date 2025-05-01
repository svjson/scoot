;;; scoot.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson (johansson.sven@gmail.com)
;; Maintainer: Sven Johansson (johansson.sven@gmail.com)
;; Version: 0.1.0
;; Package-Requires: (dependencies)
;; Homepage: https://www.github.com/svjson/scoot
;; Keywords: sql sql-client

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

(require 'request)
(require 'sql)

(defgroup scoot nil
  "Scoot-mode for SQL scratch buffers."
  :group 'tools)

(defcustom scoot-scratch-directory (expand-file-name "~/.scoot/scratches/")
  "Directory to store Scoot scratch files."
  :type 'directory)

(defvar scoot-connections (make-hash-table :test #'equal)
  "Table of known scoot connection names to connection strings.")

(defun scoot-ensure-scratch-directory ()
  "Ensure that the configured scratch directory exists."
  (unless (file-directory-p scoot-scratch-directory)
    (make-directory scoot-scratch-directory t)))

(defun scoot-add-connection (name conn-string)
  "Add a named connection to the scoot connections table.

NAME is the name of the connection.
CONN-STRING is the connection-string to use."
  (puthash name conn-string scoot-connections)
  name)

(defun scoot--read-connection-name ()
  "Prompt for a connection name with Helm if available."
  (let* ((names (hash-table-keys scoot-connections))
         (prompt "Connection name: ")
         (default (car names)))
    (if (and (featurep 'helm) names)
        (helm :sources (helm-build-sync-source "Scoot connections"
                         :candidates names
                         :fuzzy-match t)
              :buffer "*helm scoot connections*")
      (read-string prompt nil nil default))))

(defun scoot-new-scratch ()
  "Create a new Scoot scratch buffer."
  (interactive)
  (scoot-ensure-scratch-directory)
  (let* ((conn-name (scoot--read-connection-name))
         (conn-string (or (gethash conn-name scoot-connections)
                          (read-string (format "Connection string for '%s': " conn-name))))
         (_ (unless (gethash conn-name scoot-connections)
              (scoot-add-connection conn-name conn-string)))
         (scratch-name (read-string "Scratch name (default: same as connection): " nil nil conn-name))
         (filename (expand-file-name (concat scratch-name ".scoot") scoot-scratch-directory))
         (buffer (find-file-noselect filename)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "-- @connection-string: %s\n" conn-string))
      (insert (format "-- @connection-name: %s\n\n" conn-name))
      (scoot-mode)
      (setq-local scoot-connection-name conn-name))
    (pop-to-buffer buffer)))

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

(defun scoot--resolve-context-at-point ()
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

(defun scoot-eval-statement-before-point ()
  "Evaluate SQL statement before point."
  (interactive)
  (let ((ctx (scoot--resolve-context-at-point)))
    (with-temp-buffer
      (insert (car ctx))
      (goto-char (point-max))
      (let ((stmt (scoot-statement-before-point)))
        (scoot-send-to-server stmt (cdr ctx))))))

(defun scoot-eval-region (beg end)
  "Evaluate all SQL statements in the selected region.

BEG and END describe the region start and end"
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
         (stmts (scoot-split-sql-statements region)))
    (dolist (stmt stmts)
      (scoot-send-to-server stmt))))

(defun scoot--list-objects (object-type title)
  "List objects visible to the user of the current connection.

OBJECT-TYPE is the type of the object to list.
TITLE is the resultset header to be used."
  (interactive)
  (request
    (format "http://localhost:4444/api/default/%s" object-type)
    :type "GET"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((buf (get-buffer-create "*scoot-result*")))
                  (with-current-buffer buf
                    (read-only-mode -1)
                    (erase-buffer)
                    (insert (scoot--pretty-print-table (list title)
                                                       (mapcar #'list
                                                               (seq-into (alist-get object-type data) 'list))))
                    (goto-char 1))
                  (display-buffer buf))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Error: %s" data)))))

(defun scoot-list-databases ()
  "List databases visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'databases "Database Name"))

(defun scoot-list-schemas ()
  "List schemas visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'schemas "Schema Name"))

(defun scoot-list-tables ()
  "List tables visible to the user of the current connection."
  (interactive)
  (scoot--list-objects 'tables "Table Name"))

(defun scoot-describe-table (table-name)
  "Describe a database table.

If TABLE-NAME is provided, this will be used as table name."
  (interactive
   (list
    (cond
     ((use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end)))
     ((thing-at-point 'symbol t))
     (t
      (read-string "Table name: ")))))
  (request
    (format "http://localhost:4444/api/default/tables/%s" table-name)
    :type "GET"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((columns '(name type nullable primary_key default))
                      (buf (get-buffer-create "*scoot-result*")))
                  (with-current-buffer buf
                    (read-only-mode -1)
                    (erase-buffer)
                    (insert (scoot--pretty-print-table columns
                                                       (mapcar (lambda (entry)
                                                                 (mapcar (lambda (col-name)
                                                                           (alist-get col-name entry))
                                                                         columns))
                                                               (alist-get 'columns data))))
                    (goto-char 1))
                  (display-buffer buf))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Error: %s" data)))))

(defun scoot-split-sql-statements (text)
  "Naively split TEXT into SQL statements using semicolons."
  (split-string text ";[ \n]*" t))

(defun scoot--column-width (val)
  "Calculates the width of string value of VAL."
  (string-width (format "%s" val)))

(defun scoot--pretty-print-table (headers rows)
  "Return a string of a pretty-printed ASCII table.

HEADERS is a list of strings.
ROWS is a list of lists of strings."
  (let* ((columns (length headers))
         (all-rows (cons headers rows))
         (widths (apply #'cl-mapcar
                        (lambda (&rest col) (apply #'max (mapcar #'scoot--column-width col)))
                        all-rows))
         (make-row (lambda (row)
                     (concat "| "
                             (mapconcat
                              (lambda (pair)
                                (format (format "%%-%ds" (cadr pair)) (car pair)))
                              (cl-mapcar #'list row widths)
                              " | ")
                             " |")))
         (separator (concat "+-"
                            (mapconcat (lambda (w) (make-string w ?-)) widths "-+-")
                            "-+")))
    (mapconcat #'identity
               (append (list separator
                             (funcall make-row headers)
                             separator)
                       (mapcar make-row rows)
                       (list separator))
               "\n")))

(defun scoot-send-to-server (stmt &optional ctx-props)
  "Send SQL STMT to the scoot server.  Placeholder implementation.

If CTX-PROPS are provided these will be used to create, select and/or configure
a server connection, otherwise the buffer-local connection will be used."

  ;; Later we will resolve connection etc.
  (request
    "http://localhost:4444/api/default/query"
    :type "POST"
    :data (json-encode `(("sql" . ,stmt)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((buf (get-buffer-create "*scoot-result*")))
                  (with-current-buffer buf
                    (read-only-mode -1)
                    (erase-buffer)
                    (insert (scoot--pretty-print-table (seq-into (alist-get 'columns data) 'list)
                                                       (seq-into (alist-get 'rows data) 'list)))
                    (goto-char 1))
                  (display-buffer buf))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Error: %s" data)))))


(defvar scoot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'scoot-eval-statement-before-point)
    (define-key map (kbd "C-c C-r") #'scoot-eval-region)
    (define-key map (kbd "C-c s d") #'scoot-list-databases)
    (define-key map (kbd "C-c s s") #'scoot-list-schemas)
    (define-key map (kbd "C-c s t") #'scoot-list-tables)
    (define-key map (kbd "C-c d t") #'scoot-describe-table)
    map)
  "Keymap for `scoot-mode'.")

(define-derived-mode scoot-mode prog-mode "Scoot"
  "Major mode for Scoot SQL scratch buffers."
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local scoot-connection-name nil)
  (when (not (when (treesit-available-p)
               (when (treesit-language-available-p 'sql)
                 (treesit-parser-create 'sql)
                 (setq-local major-mode-remap-alist '((sql-mode . scoot-mode)))
                 (treesit-major-mode-setup)
                 t)))
    (setq-local font-lock-defaults
                '(sql-mode-ms-font-lock-keywords nil t))))

(provide 'scoot)

;;; scoot.el ends here
