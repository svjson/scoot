;;; scoot-common.wrap-string.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-common)



;; scoot--wrap-string

(ert-deftest scoot--wrap-string--multiline-statement--no-mod ()
  (setq-local test--sql "SELECT
  contactCode as cc, reviewStatus
FROM
  application_profile
INNER JOIN
  application_profile_housing_reference
ON
  application_profile.id = application_profile_housing_reference.applicationProfileId;")
  (should (equal '("SELECT"
                   "  contactCode as cc, reviewStatus"
                   "FROM"
                   "  application_profile"
                   "INNER JOIN"
                   "  application_profile_housing_reference"
                   "ON"
                   "  application_profile.id = application_profile_housing_reference.applicationProfileId;")
                 (scoot--wrap-string test--sql 100))))

(ert-deftest scoot--wrap-string--multiline-statement-with-empty-lines--no-mod ()
  (setq-local test--sql "SELECT
  contactCode as cc, reviewStatus

FROM
  application_profile
INNER JOIN
  application_profile_housing_reference

ON
  application_profile.id = application_profile_housing_reference.applicationProfileId;")
  (should (equal '("SELECT"
                   "  contactCode as cc, reviewStatus"
                   ""
                   "FROM"
                   "  application_profile"
                   "INNER JOIN"
                   "  application_profile_housing_reference"
                   ""
                   "ON"
                   "  application_profile.id = application_profile_housing_reference.applicationProfileId;")
                 (scoot--wrap-string test--sql 100))))


(ert-deftest scoot--wrap-string--multiline-statement--requires-trimming ()
  (setq-local test--sql "SELECT
  contactCode as cc, reviewStatus
FROM
  application_profile")

  (should (equal '("SELECT"
                   "  contactCode as cc,"
                   " reviewStatus"
                   "FROM"
                   "  application_profil"
                   "e")
                 (scoot--wrap-string test--sql 20))))

(ert-deftest scoot--wrap-string--multiline-statement-with-tabs--requires-trimming ()
  (setq-local tab-width 2)
  (setq-local test--sql "SELECT
	contactCode as cc, reviewStatus
FROM
	application_profile")

  (should (equal '("SELECT"
                   "	contactCode as cc,"
                   " reviewStatus"
                   "FROM"
                   "	application_profil"
                   "e")
                 (scoot--wrap-string test--sql 20))))





;;; scoot-common.wrap-string.test.el ends here
