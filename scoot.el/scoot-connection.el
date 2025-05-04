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
(require 'scoot-result)

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

(defun scoot-server-base-connection-url (&optional connection-name)
  "Construct the scoot server base url for the API and connection.

Optionally provide CONNECTION-NAME, or rely on the default name."
  (format "%s/api/%s/"
          (scoot-server--base-url)
          (or connection-name scoot-server-default-connection-name)))

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


(cl-defun scoot-connection--error-handler (&key op retry-fn retry-args &allow-other-keys)
  "Return a general purpose error handler with retry capabilities.

Arguments:
OP - The operation that caused an error.
RETRY-FN - (Optional) the function to call for retry
RETRY-ARGS - (Optional) the arguments to :fn."
  (cl-function
   (lambda (&key data error-thrown &allow-other-keys)
     (let* ((message (alist-get 'message data))
            (error (alist-get 'error data))
            (display-msg (or message error-thrown)))
       (message "Unsuccessful(%s): %s" op display-msg)
       (when (string-equal error "missing-driver")
         (let ((driver (alist-get 'driver data)))
           (scoot-server--attempt-install-driver
            driver
            (lambda ()
              (when retry-fn
                (apply retry-fn retry-args))))))))))

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

(defun scoot--send-to-server-with-connection (connection-name
                                              stmt
                                              &optional
                                              ctx-props)
  "Send SQL STMT to the scoot server using a verified connection.

The verified connection name is provided by CONNECTION-NAME.
The original request information are contained in STMT and CTX-PROPS."
  (scoot-ensure-server)
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
    :error (scoot-connection--error-handler :op 'scoot--send-to-server-with-connection)))

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

(defun scoot--register-connection (callback connection-name connection-string)
  "Registers a database connection in the Scoot Server.

CALLBACK will be called upon a successful connection made using the supplied
CONNECTION-NAME and CONNECTION-STRING.

if `scoot-auto-persist-connections` is non-nil, the connection will be persisted
to the currently active configuration of the Scoot Server."
  (scoot-ensure-server)
  (request
    (format "%s/api/connection" (scoot-server--base-url))
    :type "POST"
    :data (json-encode `((url . ,connection-string)
                         (name . ,connection-name)
                         (persist . ,scoot-auto-persist-connections)))
    :parser 'json-read
    :headers scoot--json-headers
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback)))
    :error (scoot-connection--error-handler :op 'scoot--register-connection
                                            :retry-fn #'scoot--register-connection
                                            :retry-args (list callback connection-name connection-string))))

(defun scoot--list-remote-connections (callback)
  "Query the Scoot Server for configured connections.

Successful attempts to list remote connections will invoke CALLBACK with
the remote connection collection."
  (scoot-ensure-server)
  (request
    (format "%s/api/connection" (scoot-server--base-url))
    :type "GET"
    :parser 'json-read
    :headers scoot--json-headers
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (alist-get 'connections  data))))
    :error (scoot-connection--error-handler :op 'scoot--list-remote-connections)))

(provide 'scoot-connection)

;;; scoot-connection.el ends here
