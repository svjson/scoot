;;; test-init.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; init.el file for daemonized Emacs instances used for running tests in
;; isolation from any user configuration.

;;; Code:


;; Install dependencies for scoot.el

(require 'package)
(progn
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (use-package request :ensure t)
  (use-package dash :ensure t)
  (add-to-list 'load-path (expand-file-name "scoot.el/" default-directory))
  (add-to-list 'load-path (expand-file-name "scoot.el/test" default-directory))
  "Ensured packages")



;; Set up test dependencies

(require 'ert)

(add-to-list 'load-path (expand-file-name "system_test/emacs/tests/" default-directory))
(require 'scoot-test-base)
(require 'scoot-server)
(setq scoot-show-server-buffer t)



;; Conditionally enable network debugging

(require 'cl-lib)

(defun scoot-test--log-network (orig-fun &rest args)
  "Advice function for wrapping `make-network-process`.

ORIG-FUN and ARGS should corresponed to `make-network-process`."
  (let* ((plist   (nthcdr 2 args))
         (host    (plist-get plist :host))
         (service (plist-get plist :service)))
    (when (and host service
               (or (equal host "localhost")
                   (equal host "127.0.0.1")
                   (equal host "0.0.0.0"))
               (equal (format "%s" service) "8224"))
      (message "SCOOT-NET: make-network-process host=%S service=%S ARGS=%S"
               host service args)
      (message "SCOOT-NET backtrace:\n%s"
               (with-temp-buffer
                 (let ((standard-output (current-buffer)))
                   (backtrace))
                 (buffer-string))))
    (apply orig-fun args)))

(when (getenv "SCOOT_CI")
  (message "Enabling network debugging...")
  (advice-add 'make-network-process :around #'scoot-test--log-network))



;; Test runner functions used to trigger tests and format result

(defun scoot-test--cut-after-backtrace-frame (str)
  "Trim backtrace in STR at the first occurence of nested backtrace."
  (if (string-match "\\([^\\n]#s(backtrace-frame\\)" str)
      (concat (substring str 0 (+ (match-end 1) 1)) "...")
    str))

(defun scoot-test--format-result (result)
  "Format base ert-test-result RESULT properties to an easy-to-parse format."
  (let ((messages (ert-test-result-messages result))
        (should-forms (ert-test-result-should-forms result))
        (duration (ert-test-result-duration result)))
    (concat (format "[Result]\n%s\n[/Result]\n" (type-of result))
            (format "[Duration]\n%s\n[/Duration]\n" duration)
            (format "[Messages]\n%s\n[/Messages]\n" messages)
            (format "[Assertions]\n%s\n[/Assertions]\n"
                    (mapconcat
                     (lambda (form)
                       (concat "[Assertion]\n"
                               (let ((pl (cdr form)))
                                 (apply
                                  #'concat
                                  (cl-loop for (key value) on pl by #'cddr
                                           collect (format "[%s]\n%s[/%s]\n"
                                                           key
                                                           (pp-to-string value)
                                                           key))))
                               "[/Assertion]"))
                     should-forms
                     "\n")))))

(defun scoot-test--format-result-with-cond (result)
  "Format failed ert-test-result RESULT properties to an easy-to-parse formed."
  (let ((backtrace (ert-test-result-with-condition-backtrace result))
        (infos (ert-test-result-with-condition-infos result)))
    (concat (scoot-test--format-result result)
            (format "[Backtrace]\n%s\n[/Backtrace]\n"
                    (let ((frames '())
                          (rest backtrace))
                      (while (and rest (< (length frames) 10))
                        (setq frames (append frames
                                             (list (scoot-test--cut-after-backtrace-frame (pp-to-string (car rest))))))
                        (setq rest (cdr rest)))
                      (string-join frames "\n")))
            (format "[Infos]\n%s\n[Infos]\n" (mapconcat #'identity infos "\n")))))

(defun scoot-test--run-test (test-name)
  "Run ert test TEST-NAME and return the test output."
  (let* ((result (ert-run-test (ert-get-test test-name))))
    (concat (cond
             ((ert-test-result-type-p result (or :passed :skipped))
              (scoot-test--format-result result))
             (t (scoot-test--format-result-with-cond result)))
            (if-let ((server-buf (get-buffer scoot-server-buffer-name))
                     (live-p (buffer-live-p server-buf)))
                (format "[ServerOutput]\n%s\n[/ServerOutput]\n"
                        (with-current-buffer scoot-server-buffer-name
                          (buffer-string)))
              ""))))



;; Attempt to clean up stray gpg-agents and scdaemons on exit

(defun scoot-test--cleanup-gpg ()
  "Terminate gpg-agent and scdaemon."
  (let ((gpg-agent-info (getenv "GPG_AGENT_INFO")))
    (when gpg-agent-info
      (let ((pid (string-to-number (nth 1 (split-string gpg-agent-info ":")))))
        (ignore-errors (kill-process pid)))))
  (ignore-errors (call-process "pkill" nil nil nil "-f" "scdaemon")))

(add-hook 'kill-emacs-hook #'scoot-test--cleanup-gpg)


;; Export the PID of the emaces daemon so we can predictably kill it if it
;; refuses to exit gracefully.

(with-temp-file (expand-file-name "proc.pid" user-emacs-directory)
  (princ (emacs-pid) (current-buffer)))


;;; test-init.el ends here
;; Local Variables:
;; flycheck-emacs-lisp-checkdoc-include-package-file: nil
;; End:
