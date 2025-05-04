;;; scoot.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson (johansson.sven@gmail.com)
;; Maintainer: Sven Johansson (johansson.sven@gmail.com)
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (request "20250219") (sql "3.0))
;; Homepage: https://www.github.com/svjson/scoot
;; Keywords: sql, database, sql-client, tools, convenience

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

;; Scoot is a SQL client for Emacs that interacts with a persistent backend server.
;; It supports scratch buffers, result metadata, connection management, and editing.

;;; Code:

(require 'request)
(require 'sql)
(require 'scoot-result)

(defgroup scoot nil
  "Scoot-modes for SQL interaction via SQL scratch buffers and result set buffers."
  :group 'tools)

(defcustom scoot-scratch-directory (expand-file-name "~/.scoot/scratches/")
  "Directory to store Scoot scratch files."
  :type 'directory
  :group 'scoot)

(defcustom scoot-server-host "localhost"
  "The hostname of the Scoot Server."
  :type 'string
  :group 'scoot)

(defcustom scoot-server-port 8224
  "The port of the Scoot Server."
  :type 'integer
  :group 'scoot)

(defcustom scoot-server-default-connection-name "default"
  "The name of the default connection name to use, if not provided."
  :type 'string
  :group 'scoot)

(defcustom scoot-auto-persist-connections nil
  "Determines if database connections should automatically be persisted.
If `t` any connection created by scoot.el will be added to the currently
active configuration of the Scoot Server."
  :type 'boolean
  :group 'scoot)

(defvar scoot-connections (make-hash-table :test #'equal)
  "Table of known scoot connection names to connection strings.")

(defconst scoot--json-headers '(("Content-Type" . "application/json")
                                ("Accept" . "application/json")))

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
  (when-let* ((conn-name (scoot--read-connection-name))
              (conn-string (or (gethash conn-name scoot-connections)
                               (read-string (format "Connection string for '%s': " conn-name))))
              (_ (unless (gethash conn-name scoot-connections)
                   (scoot-add-connection conn-name conn-string)))
              (scratch-name (read-string "Scratch name (default: same as connection): " nil nil conn-name))
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

(defun with-contextual-connection (callback)
  "Execute a callback/function after ensuring a connection.

CALLBACK is the function to be called after verifying the connection."
  (let* ((ctx (scoot--resolve-context-at-point))
         (ctx-props (cdr ctx))
         (connection-name (gethash "connection-name"
                                   ctx-props
                                   scoot-server-default-connection-name))
         (connection-string (gethash "connection-string"
                                     ctx-props
                                     nil)))
    (scoot--ensure-connection
     (lambda ()
       (funcall callback connection-name))
     connection-name
     connection-string)))

(defun scoot--list-objects (object-type title)
  "List objects visible to the user of the current connection.

OBJECT-TYPE is the type of the object to list.
TITLE is the resultset header to be used."
  (interactive)
  (with-contextual-connection
   (lambda (connection-name)
     (request
       (format "%s/%s"
               (scoot-server-base-connection-url connection-name)
               object-type)
       :type "GET"
       :headers scoot--json-headers
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (scoot--open-result-buffer
                    (list :type 'objects
                          :object-type object-type
                          :result
                          `((columns . [,title])
                            (rows . ,(mapcar #'vector
                                             (seq-into
                                              (alist-get object-type data)
                                              'vector)))
                            (metadata . ((columns . [((name . ,title)
                                                      (type . "OBJECT-NAME"))]))))
                          :connection connection-name))))
       :error (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Error: %s" data)))))))

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
  (with-contextual-connection
   (lambda (connection-name)
     (request
       (format "%s/tables/%s"
               (scoot-server-base-connection-url connection-name)
               table-name)
       :type "GET"
       :headers scoot--json-headers
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let ((columns '(name type nullable primary_key default)))
                     (scoot--open-result-buffer
                      (list :type 'object
                            :object-type 'table
                            :object-name (alist-get 'name data)
                            :connection connection-name
                            :result `((columns . ,columns)
                                      (rows . ,(mapcar (lambda (entry)
                                                         (mapcar (lambda (col-name)
                                                                   (alist-get col-name entry))
                                                                 columns))
                                                       (alist-get 'columns data)))
                                      (metadata . ((columns . [((name . "Name")
                                                                (type . "OBJECT-NAME"))
                                                               ((name . "Type")
                                                                (type . "DATA-TYPE"))
                                                               ((name . "Nullable")
                                                                (type . "BOOLEAN"))
                                                               ((name . "Primary Key")
                                                                (type . "BOOLEAN"))
                                                               ((name . "Default")
                                                                (type . "self(Type)"))])))))))))
       :error (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Error: %s" data)))))))

(defun scoot-split-sql-statements (text)
  "Naively split TEXT into SQL statements using semicolons."
  (split-string text ";[ \n]*" t))

(defun scoot-server-base-url ()
  "Construct the scoot server base url."
  (format "http://%s:%s"
          scoot-server-host
          scoot-server-port))

(defun scoot-server-base-connection-url (&optional connection-name)
  "Construct the scoot server base url for the API and connection.

Optionally provide CONNECTION-NAME, or rely on the default name."
  (format "%s/api/%s/"
          (scoot-server-base-url)
          (or connection-name scoot-server-default-connection-name)))

(defun scoot--list-remote-connections (callback)
  "Query the Scoot Server for configured connections.

Successful attempts to list remote connections will invoke CALLBACK with
the remote connection collection."
  (request
    (format "%s/api/connection" (scoot-server-base-url))
    :type "GET"
    :parser 'json-read
    :headers scoot--json-headers
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (alist-get 'connections  data))))
    :error (cl-function
              (lambda (&key data &allow-other-keys)
                (message (format "Unsuccessful: scoot--list-remote-connections: %s" data))))))

(defun scoot--register-connection (callback connection-name connection-string)
  "Registers a database connection in the Scoot Server.

CALLBACK will be called upon a successful connection made using the supplied
CONNECTION-NAME and CONNECTION-STRING.

if `scoot-auto-persist-connections` is non-nil, the connection will be persisted
to the currently active configuration of the Scoot Server."
  (request
    (format "%s/api/connection" (scoot-server-base-url))
    :type "POST"
    :data (json-encode `((url . ,connection-string)
                         (name . ,connection-name)
                         (persist . ,scoot-auto-persist-connections)))
    :parser 'json-read
    :headers scoot--json-headers
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback)))
    :error (cl-function
            (lambda (&key data error-thrown &allow-other-keys)
              (message (format "Unsuccessful: scoot--register-connection: %s %s" data error-thrown))))))

(defun scoot--ensure-connection (callback
                                 connection-name
                                 &optional
                                 connection-string)
  "Ensure that the Scoot Server has configured the connection CONNECTION-NAME.

When the connection is verified to be configured and ready on the server,
CALLBACK will be called with the resulting connection as an argument to
allow execution to resume.

Optionally provide CONNECTION-STRING to create the connection if not present,
or configured towards another target."
  (if connection-string
      (scoot--register-connection (lambda ()
                                    (funcall callback))
                                  connection-name
                                  connection-string)
    (scoot--list-remote-connections
     (lambda (remote-connections)
       (let ((connection (alist-get (intern connection-name)
                                    remote-connections
                                    nil)))
         (if connection
             (progn  (puthash connection-name
                              (or connection-string
                                  (gethash connection-name scoot-connections nil))
                              scoot-connections)
                     (funcall callback))
           (message "Unknown connection: %s" connection-name)))))))

(defun scoot-send-to-server (stmt &optional ctx-props)
  "Send SQL STMT to the scoot server.  Placeholder implementation.

If CTX-PROPS are provided these will be used to create, select and/or configure
a server connection, otherwise the buffer-local connection will be used."

  (let ((connection-name (gethash "connection-name"
                                  ctx-props
                                  scoot-server-default-connection-name))
        (connection-string (gethash "connection-string" ctx-props)))

    (scoot--ensure-connection (lambda ()
                                (scoot--send-to-server-with-connection
                                 connection-name
                                 stmt
                                 ctx-props))
                              connection-name
                              connection-string)))

(defun scoot--send-to-server-with-connection (connection-name
                                              stmt
                                              &optional
                                              ctx-props)
  "Send SQL STMT to the scoot server using a verified connection.

The verified connection name is provided by CONNECTION-NAME.
The original request information are contained in STMT and CTX-PROPS."
  (request
    (format "%s/query" (scoot-server-base-connection-url connection-name))
    :type "POST"
    :data (json-encode `(("sql" . ,stmt)
                         ("metadata" . t)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (scoot--open-result-buffer
                 (list :type 'query
                       :result data
                       :connection connection-name
                       :statement stmt))))
    :error (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Error: %s" data)))))


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

(define-derived-mode scoot-scratch-mode prog-mode "Scoot Scratch"
  "Major mode for Scoot SQL scratch buffers."
  (setq-local comment-start "-- ")
  (setq-local comment-end "")
  (setq-local scoot-connection-name nil)
  (when (not (when (treesit-available-p)
               (when (treesit-language-available-p 'sql)
                 (treesit-parser-create 'sql)
                 (setq-local major-mode-remap-alist '((sql-mode . scoot-scratch-mode)))
                 (treesit-major-mode-setup)
                 t)))
    (setq-local font-lock-defaults
                '(sql-mode-ms-font-lock-keywords nil t))))

(provide 'scoot)

;;; scoot.el ends here
