;;; name.el --- summary -*- lexical-binding: t -*-

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

(require 'scoot)
(require 'scoot-test-base)

(ert-deftest result--verify-column-headers-with-primary-and-foreign-keys ()
  (customize-set-value 'scoot-primary-key-icon "PK ")
  (customize-set-value 'scoot-foreign-key-icon "FK ")
  (unwind-protect
      (let ((result-buf nil))
        (scoot-connection--register-connection
         (lambda (connection)
           (scoot-connection--execute-statement
            connection
            "SELECT * FROM nexartrade_staging.orders"
            (lambda (result-context)
              (message "%s" result-context)
              (setq result-buf (scoot-result--open-result-buffer result-context)))))
         scoot-test--connection-name
         scoot-test--connection-string)

        (with-timeout (2 (error "Test timed out"))
          (while (null result-buf)
            (sleep-for 0.1)))

        (with-current-buffer result-buf
          (goto-char (point-min))
          (search-forward "+--")
          (beginning-of-line)
          (unwind-protect
              (should
               (string-equal
                (scoot-string-depropertize (scoot-get-buffer-lines (point) 3))
                (concat
                 "+-------+------------+--------------+--------+------------------+--------------+------------+\n"
                 "| PK id | FK user_id | order_number | status | shipping_address | total_amount | created_at |\n"
                 "+-------+------------+--------------+--------+------------------+--------------+------------+\n")))
            (when (buffer-live-p result-buf)
              (kill-buffer result-buf)))))
    (custom-reevaluate-setting 'scoot-primary-key-icon)
    (custom-reevaluate-setting 'scoot-foreign-key-icon)))


;;; column-tests.el ends here
