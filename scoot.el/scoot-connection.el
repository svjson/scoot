;;; scoot-connection.el --- summary -*- lexical-binding: t -*-

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

;; Implements connection management for Scoot by interfacing with the
;; scoot-server to track and control SQL connection configurations used by
;; Scoot buffers.
;;
;; Acts as a client-side representation of the server's connection state,
;; maintaining a local cache of known connections and providing utilities
;; for querying, selecting, and assigning them to Scoot buffers.
;;
;; Connections may also be defined within Emacs and pushed to the server,
;; allowing Emacs to serve as both a consumer and source of connection
;; definitions.  This module provides the infrastructure for consistent
;; connection resolution and state tracking across all Scoot features that
;; rely on an active SQL connection.

;;; Code:

(require 'request)
(require 'scoot-server)
(require 'scoot-common)

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

;;;###autoload
(defvar scoot-connections (make-hash-table :test #'equal)
  "Table of known scoot connection names to connection strings.")

(defconst scoot--json-headers '(("Content-Type" . "application/json")
                                ("Accept" . "application/json")))

(defun scoot-server-base-connection-url (&optional connection-name)
  "Construct the scoot server base url for the API and connection.

Optionally provide CONNECTION-NAME, or rely on the default name."
  (format "%s/api/%s"
          (scoot-server--base-url)
          (or connection-name scoot-server-default-connection-name)))

;;;###autoload
(defun scoot-add-connection (name conn-string)
  "Add a named connection to the scoot connections table.

NAME is the name of the connection.
CONN-STRING is the connection-string to use."
  (let ((connection (list :name name :url conn-string)))
    (puthash name connection
             scoot-connections)
    connection))

(defun scoot-connection--store-connection (connection)
  "Store a complete CONNECTION with metadata to the scoot connections table."
  (puthash (plist-get connection :name) connection scoot-connections))

(defun scoot-connection--connection-annotation-fn (connection-name)
  "Annotate CONNECTION-NAME for display `completing-read`."
  (if-let (conn (gethash connection-name scoot-connections nil))
      (format "  %s" conn)
    "  (Not initialized)"))

(cl-defun scoot-connection--connection-prompt (&key connection-name &allow-other-keys)
  "Prompt the user for a connection name.

Optionally provide a default selection with CONNECTION-NAME."
  (let* ((names (hash-table-keys scoot-connections))
         (default (or connection-name (car names))))
    (scoot--completing-read :name "Scoot Connections"
                            :prompt "Connection: "
                            :candidates names
                            :default-value default
                            :sort-fn #'string<
                            :annotation-fn 'scoot-connection--connection-annotation-fn)))

(cl-defun scoot-connection--table-prompt (&key table-name tables &allow-other-keys)
  "Prompt the user for a table name.

Optionally provide a default selection with TABLE-NAME.

Provide a list of valid options in TABLES for completing read action`"
  (if tables
      (scoot--completing-read :name "Scoot Tables"
                              :prompt "Table: "
                              :candidates tables
                              :sort-fn #'string<)
    (read-string "Table name: " nil nil table-name)))

(cl-defun scoot-connection--error-handler (&key op url retry-fn retry-args connection &allow-other-keys)
  "Return a general purpose error handler with retry capabilities.

Arguments:
OP - The operation that caused an error.
URL - The URL that the failed request was made to.
RETRY-FN - (Optional) the function to call for retry
RETRY-ARGS - (Optional) the arguments to :fn.
CONNECTION - (Optional) Used to register connections unknown to the server."
  (cl-function
   (lambda (&key data error-thrown &allow-other-keys)
     (let* ((message (alist-get 'message data))
            (error (alist-get 'error data))
            (display-msg (or message error-thrown))
            (unhandled-error-msg (lambda ()
                                   (message "Unsuccessful(%s): %s"
                                            (cond
                                             ((and op url) (format "%s: %s" op url))
                                             (op op)
                                             (url url)
                                             (t "unknown"))
                                            display-msg))))

       (pcase error
         ("query-error" (message "%s" display-msg))
         ("unknown-connection" (if (and retry-fn connection)
                                   (scoot-connection--register-connection
                                    (lambda (_)
                                      (apply retry-fn retry-args))
                                    (plist-get connection :name)
                                    (plist-get connection :url))
                                 (funcall unhandled-error-msg)))
         (_ (funcall unhandled-error-msg)))
       (when (string-equal error "missing-driver")
         (let ((driver (alist-get 'driver data)))
           (scoot-server--attempt-install-driver
            driver
            (lambda ()
              (when retry-fn
                (apply retry-fn retry-args))))))))))

(defun scoot-connection--ensure-connection-by-name-and-string (connection-name
                                                               connection-string
                                                               callback)
  "Ensure that the Scoot Server has configured the connection CONNECTION-NAME.

When the connection is verified to be configured and ready on the server,
CALLBACK will be called with the resulting connection as an argument to
allow execution to resume.

Optionally provide CONNECTION-STRING to create the connection if not present,
or configured towards another target."
  (if connection-string
      (scoot-connection--register-connection (lambda (connection)
                                               (funcall callback connection))
                                             connection-name
                                             connection-string)
    (progn
      (scoot-connection--list-remote-connections
       (lambda (remote-connections)
         (let ((connection (cdr (assoc connection-name
                                       remote-connections)))
               (local-conn (gethash connection-name scoot-connections nil)))
           (if connection
               (progn
                 (puthash connection-name
                          (scoot--plist-merge local-conn connection)
                          scoot-connections)
                 (funcall callback connection))
             (message "Unknown connection and no connection details available: %s"
                      connection-name))))))))

(cl-defun scoot-connection--send-request (&key uri
                                               callback
                                               (method "GET")
                                               body
                                               (headers scoot--json-headers)
                                               retry-fn
                                               retry-args
                                               op
                                               connection)
  "Send an http request to the scoot-server.

This is the lowest-level http request method provided by scoot--connection,
and all http requests are meant to use this.

URI is the remote address to be called.  The host, port and protocol will be
provied by `scoot-server--base-url`.

CALLBACK is the \"success callback\" that will be invoked and passed the
response data upon a successful request.

METHOD specifies the http method to use for the request. Defaults to GET.

BODY contains the request body and may be a string, an alist or nil.

HEADERS may be passed to override the default of `scoot--json-headers`.

RETRY-FN and RETRY-ARGS may be passed to provide a method to retry a failed
request that is deemed to be retryable. Any such function should attempt to
resolve the cause of the failure before trying again.

OP is a symbol describing the operation that this request is a part of.

CONNECTION can optionally be supplied to assist the error handler in retrying
recoverable errors."
  (scoot-ensure-server)
  (let ((url (format "%s%s" (scoot-server--base-url) uri)))
    (request (url-encode-url url)
      :type method
      :headers headers
      :data (cond
             ((stringp body)
              body)
             ((and (not (null body))
                   (listp body))
              (json-encode body)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error (scoot-connection--error-handler :op op
                                              :url url
                                              :retry-fn retry-fn
                                              :retry-args retry-args
                                              :connection connection)
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (when-let ((buf (request-response--buffer response)))
                     (when (buffer-live-p buf)
                       (kill-buffer buf))))))))

(defun scoot-connection--statement-operation (connection stmt request-op action callback)
  "Send a SQL STMT operation to the scoot server using a verified connection.

Operations include actions like \"execute\" and \"modify\".

The verified connection is provided by the plist CONNECTION.
STMT is the statement to operate on.
REQUEST-OP is a symbol describing the purpose of the server request.
ACTION is an alist describing the action of the operation..
CALLBACK will be invoked with the result if the operation is successful."
  (scoot-ensure-server)
  (let ((connection-name (plist-get connection :name)))
    (scoot-connection--send-request
     :connection connection
     :op request-op
     :uri (format "/api/%s/query" connection-name)
     :method "POST"
     :body `(("sql" . ,stmt)
             ("metadata" . t)
             ("action" . ,action))
     :retry-fn #'scoot-connection--statement-operation
     :retry-args (list connection stmt request-op action callback)
     :callback (lambda (data)
                 (funcall
                  callback
                  (list :type 'query
                        :result data
                        :connection connection-name
                        :statement stmt))))))

(defun scoot-connection--execute-statement (connection stmt callback)
  "Send SQL STMT to the scoot server using a verified connection.

The verified connection is provided by the plist CONNECTION.
STMT is the statement to execute.
CALLBACK will be invoked with the result if the operation is successful."
  (scoot-connection--statement-operation
   connection
   stmt
   'execute-statement
   '((action . "execute"))
   callback))

(defun scoot-connection--modify-statement (connection stmt modify-op modify-conds callback)
  "Request to modify SQL STMT to the scoot server using a verified connection.

The verified connection is provided by the plist CONNECTION.
STMT is the statement to modify.
MODIFY-OP is a tuple describing what to modify, ie (`WHERE `add).
MODIFY-CONDS is a list of modifications, ie (\"account_id\" \"=\" \"5\").
CALLBACK will be invoked with the result if the operation is successful."
  (scoot-connection--statement-operation
   connection
   stmt
   'modify-statement
   (list (cons 'action "modify")
         (cons 'target (car modify-op))
         (cons 'operation (cadr modify-op))
         (cons 'conditions (pcase (car modify-op)
                             ("SELECT" modify-conds)
                             ("WHERE" (list (list (cons 'lhs (nth 0 modify-conds))
                                                  (cons 'cmp (nth 1 modify-conds))
                                                  (cons 'rhs (nth 2 modify-conds))))))))
   callback))

(defun scoot-connection--list-objects (connection object-type callback)
  "List objects of type OBJECT-TYPE visible to connection CONNECTION.

Invokes CALLBACK with the result if successful."

  (scoot-ensure-server)
  (scoot-connection--send-request
   :uri (format "/api/%s/%s"
                (plist-get connection :name)
                object-type)
   :op (intern (concat "list-" (symbol-name object-type)))
   :callback callback))

(defun scoot-connection--register-connection (callback connection-name connection-string)
  "Registers a database connection in the Scoot Server.

CALLBACK will be called upon a successful connection made using the supplied
CONNECTION-NAME and CONNECTION-STRING.

if `scoot-auto-persist-connections` is non-nil, the connection will be persisted
to the currently active configuration of the Scoot Server."
  (scoot-ensure-server)
  (scoot-connection--send-request
   :uri "/api/connection"
   :method "POST"
   :body `((url . ,connection-string)
           (name . ,connection-name)
           (persist . ,scoot-auto-persist-connections))
   :callback (lambda (data)
               (funcall callback
                        (scoot-connection--store-connection
                         (scoot--plist-merge
                          (scoot--alist-to-plist (alist-get 'connection data))
                          (list :name connection-name
                                :url connection-string)))))
   :retry-fn #'scoot-connection--register-connection
   :retry-args (list callback connection-name connection-string)))

(defun scoot-connection--list-remote-connections (callback)
  "Query the Scoot Server for configured connections.

Successful attempts to list remote connections will invoke CALLBACK with
the remote connection collection."
  (scoot-ensure-server)
  (scoot-connection--send-request
   :uri "/api/connection"
   :callback (lambda (data)
               (let ((connections (alist-get 'connections data)))
                 (funcall
                  callback
                  (mapcar
                   (lambda (entry)
                     (let* ((name (symbol-name (car entry)))
                            (conn (cdr entry)))
                       (cons name (scoot--plist-merge
                                   (list :name name)
                                   (scoot--alist-to-plist conn)))))
                   connections))))))

(defun scoot-connection--describe-table (connection table-name callback)
  "Send a request to the scoot server to describe table TABLE-NAME.

CONNECTION needs to have been resolved and verified before calling
this function.

If the request is successful CALLBACK will be called with the formatted
result."
  (scoot-ensure-server)
  (let ((connection-name (plist-get connection :name)))
    (scoot-connection--send-request
     :uri (format "/api/%s/tables/%s"
                  connection-name
                  table-name)
     :op 'describe-table
     :callback (lambda (data)
                 (let ((columns '(name type nullable primary_key default)))
                   (funcall
                    callback
                    (list :type 'object
                          :object-type 'table
                          :object-name (alist-get 'name data)
                          :connection connection-name
                          :result `((columns . ,columns)
                                    (rows . ,(mapcar (lambda (entry)
                                                       (mapcar (lambda (col-name)
                                                                 (if (equal col-name 'type)
                                                                     (alist-get 'native_type entry)
                                                                   (alist-get col-name entry)))
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
                                                              (type . "self(Type)"))])
                                                 (sql . (("CREATE TABLE-statement" . ,(alist-get 'create_stmt data)))))))))))
     :connection connection
     :retry-fn #'scoot-connection--describe-table
     :retry-args (list connection table-name callback))))

(provide 'scoot-connection)

;;; scoot-connection.el ends here
