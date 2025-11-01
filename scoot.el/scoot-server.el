;;; scoot-server.el --- summary -*- lexical-binding: t -*-

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

;; This file provides Emacs-side management of the Scoot server,
;; which powers the Emacs integration in scoot.el.

;; The functions here are responsible for:
;; - Ensuring a virtualenv exists for the Scoot Python backend.
;; - Installing Scoot’s Python components in editable mode (core, server).
;; - Starting the scoot-server process according to the configuration.
;; - Polling to confirm server readiness.
;; - Stopping or restarting the server cleanly from within Emacs.

;; These utilities are designed to be internal to the Emacs integration,
;; but can be invoked directly if needed (e.g. via `M-x scoot-start-server`).

;; By default, output from the server process  is hidden unless
;; `scoot-show-server-buffer` is set to non-nil.

;;; Code:

(defcustom scoot-auto-start-server t
  "If non-nil, automatically start the Scoot server if not running."
  :type 'boolean
  :group 'scoot)

(defcustom scoot-server-host "localhost"
  "The hostname of the Scoot Server."
  :type 'string
  :group 'scoot)

(defcustom scoot-server-port 8224
  "The port of the Scoot Server."
  :type 'integer
  :group 'scoot)

(defcustom scoot-server-config-name "default"
  "The default configuration name of the scoot-server."
  :type 'string
  :group 'scoot)

(defcustom scoot-show-server-buffer nil
  "If non-nil, show the Scoot server output in a visible buffer."
  :type 'boolean
  :group 'scoot)

(defcustom scoot-server-buffer-name "*scoot-server*"
  "The name of the Scoot server output buffer."
  :type 'string
  :group 'scoot)

(defvar scoot-server-start-timeout 5
  "Timeout for waiting for a managed Scoot server to become responsive.")

(defvar scoot-server--process nil
  "The Emacs process object for the running Scoot server.")

(defun scoot--project-root ()
  "Return the root directory where Scoot is installed."
  (let ((lib-path (file-name-directory (locate-library "scoot"))))
    (if (string-match-p "/straight/build" lib-path)
        (expand-file-name "../../repos/scoot" lib-path)
      (expand-file-name ".." lib-path))))

(defun scoot--venv-dir ()
  "Return the python virtual environment root."
  (expand-file-name ".venv" (scoot--project-root)))

(defun scoot--core-dir ()
  "Return the path the the scoot core package root."
  (expand-file-name "core" (scoot--project-root)))

(defun scoot--server-dir ()
  "Return the path the the scoot server package root."
  (expand-file-name "server" (scoot--project-root)))

(defun scoot-server--scoot-server-bin ()
  "Return the full path to the scoot-server binary."
  (expand-file-name "bin/scoot-server" (scoot--venv-dir)))

(defun scoot-server--base-url ()
  "Construct the scoot server base url."
  (format "http://%s:%s"
          scoot-server-host
          scoot-server-port))

(defun scoot-server--running-p ()
  "Return non-nil if the Scoot server appears to be running."
  (let (url-buf)
    (unwind-protect
        (when (setq url-buf (url-retrieve-synchronously (format "%s/ping" (scoot-server--base-url)) t t 1))
          (with-current-buffer url-buf
            (goto-char (point-min))
            (re-search-forward "200 OK" nil t)))
      (when url-buf (kill-buffer url-buf)))))

(defun scoot-server--venv-operation (op)
  "Resolve venv root, python binary and pip binary and then execute OP."
  (let* ((venv (scoot--venv-dir))
         (python (expand-file-name "bin/python" venv))
         (pip (expand-file-name "bin/pip" venv)))
    (funcall op venv python pip (scoot--project-root))))

(defun scoot-server--ensure-venv ()
  "Ensure the virtualenv and dependencies are installed."
  (message "Ensure scoot-server virtual environment...")
  (scoot-server--venv-operation
   (lambda (venv python pip _)
     ;; Create virtual environment if it doesn't exist
     (unless (file-executable-p python)
       (message "Creating .venv for Scoot...")
       (call-process "python3" nil nil nil "-m" "venv" venv))
     ;; Install scoot-core and scoot-server as editable packages quietly
     (dolist (pkg `(("scoot-core" . ,(scoot--core-dir))
                    ("scoot-server" . ,(scoot--server-dir))))
       (let* ((pkg-name (car pkg))
              (pkg-path (cdr pkg))
              (check-exit (call-process pip nil nil nil "show" pkg-name)))
         (unless (zerop check-exit)
           (message "Installing %s..." pkg-name)
           (call-process pip nil nil nil "install" "-e" pkg-path)))))))

(defun scoot--build-server-args ()
  "Build the arguments to pass to scoot-server."
  (let (args)
    (when scoot-server-port
      (setq args (append args (list (format "-p%d" scoot-server-port)))))
    (when scoot-server-config-name
      (setq args (append args (list (format "-c%s" scoot-server-config-name)))))
    args))

(defun scoot--wait-for-server (timeout)
  "Wait until the server responds to /ping or TIMEOUT (in seconds) is reached.
Returns t if the server came up in time, nil otherwise."
  (let ((start-time (float-time))
        (ready nil))
    (while (and (not ready)
                (< (- (float-time) start-time) timeout))
      (when (scoot-server--running-p)
        (setq ready t))
      (unless ready
        (sleep-for 0.1)))
    ready))

(defun scoot-server--attempt-install-driver (driver-name &optional success-callback failure-callback)
  "Attempt to install a scoot-server driver dependency named by DRIVER-NAME.

SUCCESS-CALLBACK and/or FAILURE-CALLBACK can optionally be provided."
  (scoot-server--venv-operation
   (lambda (_ _ pip scoot-root)
     (message "Installing scoot-server dependency '%s'..." driver-name)
     (with-temp-buffer
       (let ((target (expand-file-name
                      (format "server/.[%s]" driver-name)
                      scoot-root)))
         (if (zerop (call-process pip nil t nil "install" target))
             (progn
               (message "Installation successful.")
               (when success-callback
                 (funcall success-callback)))
           (progn
             (message "Failed to install scoot-server dependency '%s':\n%s install %s\n%s"
                      driver-name
                      pip
                      target
                      (buffer-string))
             (when failure-callback
               (funcall failure-callback)))))))))

;;;###autoload
(defun scoot-start-server ()
  "Manually start the Scoot server."
  (interactive)
  (unless (scoot-server--running-p)
    (scoot-server--ensure-venv)
    (message "Starting scoot-server...")
    (let* ((server-bin (scoot-server--scoot-server-bin))
           (args (scoot--build-server-args))
           (default-directory (scoot--server-dir))
           (buffer (if scoot-show-server-buffer scoot-server-buffer-name nil)))
      (setq scoot-server--process
            (apply #'start-process "scoot-server" buffer server-bin args))
      (if (scoot--wait-for-server scoot-server-start-timeout)
          (message "Scoot server started on port %s" scoot-server-port)
        (error "Scoot server failed to respond within %s seconds"
               scoot-server-start-timeout)))))

;;;###autoload
(defun scoot-stop-server ()
  "Stop the running Scoot server, if any."
  (interactive)
  (if (process-live-p scoot-server--process)
      (progn
        (message "Stopping Scoot server...")
        (delete-process scoot-server--process)
        (setq scoot-server--process nil)
        (message "Scoot server stopped."))
    (message "No running managed Scoot server.")))

;;;###autoload
(defun scoot-restart-server ()
  "Restart the Scoot server cleanly."
  (interactive)
  (if (process-live-p scoot-server--process)
      (progn
        (scoot-stop-server)
        (scoot-start-server))
    (scoot-stop-server)))

;;;###autoload
(defun scoot-ensure-server (&optional force-start)
  "Ensure that the Scoot server is running.  Start it if it is not.

Calling this function interactively or with a non-nil FORCE-START will always
make an attempt to start the server, otherwise `scoot-auto-start-server` will
be honored."
  (interactive (list t))
  (when (or scoot-auto-start-server force-start)
    (unless (scoot-server--running-p)
      (scoot-start-server))))

(provide 'scoot-server)

;;; scoot-server.el ends here
