;;; scoot-resultset.query-execution.system.test.el --- summary -*- lexical-binding: t -*-

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

(require 'ert)
(require 'scoot)
(require 'scoot-test-base)
(require 'scoot-test-fixtures)




(ert-deftest result--modify-and-and-execute-query--nexartrade-products ()
  (with-alphanum-keys
   (with-result-buffer ((query "SELECT * FROM nexartrade_staging.products")
                        (keep-buffer t))

     (should (equal (scoot--plist-select-keys (scoot-buffer-state) :major-mode :minor-modes)
                    (list :major-mode 'scoot-resultset-mode
                          :minor-modes '(read-only-mode cursor-sensor-mode))))
     (should (equal (scoot-resultset-row-identities)
                    '(1 2 3 4 5 6 7 8 9 10)))

     (do-command #'beginning-of-buffer)
     (let ((query-block (scoot-widget--get-widget :type 'query-block
                                                  :name 'query-block)))
       ;; Move to the end of the current query
       (interactively-goto-char (plist-get query-block :widget-start))

       ;; Ensure query block mode
       (wait-until (bound-and-true-p scoot-query-block-mode))


       ;; Add a WHERE-clause
       (do-command #'move-end-of-line)
       (interactively-self-insert-text " WHERE id > 5")

       ;; Execute the query
       (do-command #'scoot-rs--execute-query)

       ;; Wait for result
       (wait-until (length< (scoot-resultset-row-identities) 10))

       ;; Then expected row IDs are present
       (should (equal (scoot-resultset-row-identities)
                      '(6 7 8 9 10)))))))



;;; scoot-resultset.query-execution.system.test.el ends here
