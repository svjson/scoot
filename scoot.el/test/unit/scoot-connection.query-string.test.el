;;; scoot-connection.query-string.test.el --- summary -*- lexical-binding: t -*-

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
(require 'ert-parametrized)
(require 'scoot-connection)



(ert-parametrized-deftest scoot-connection--build-query-string
    (params-plist expected)
    (("Empty list yields empty string"
      (:eval nil)
      (:eval ""))
     ("Single parameter"
      (:eval (list :include-row-count t))
      (:eval "?include_row_count=t"))
     ("Multiple parameters"
      (:eval (list :include-row-count t
                   :max-rows 1234))
      (:eval "?include_row_count=t&max_rows=1234"))
     ("Multivalue parameters"
      (:eval (list :include "row_count"
                   :include "modified"))
      (:eval "?include=row_count&include=modified")))
  (should (equal (scoot-connection--build-query-string params-plist)
                 expected)))





;;; scoot-connection.query-string.test.el ends here
