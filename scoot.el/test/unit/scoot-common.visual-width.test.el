;;; scoot-common.visual-width.test.el --- summary -*- lexical-binding: t -*-

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



(ert-deftest scoot--visible-width--regular-string ()
  (should (equal 6 (scoot--visible-width "SELECT"))))

(ert-deftest scoot--visible-width--2-spaces-count-as-2-spaces ()
  (should (equal 12 (scoot--visible-width "HELLO  WORLD"))))

(ert-deftest scoot--visible-width--4-spaces-count-as-4-spaces ()
  (should (equal 14 (scoot--visible-width "HELLO    WORLD"))))

(ert-deftest scoot--visible-width--tab-width-2--tab-counts-as-1-space-when-just-before-tab-stop ()
  (should (equal 11 (scoot--visible-width "HELLO	WORLD" 2))))

(ert-deftest scoot--visible-width--tab-width-4--tab-counts-as-1-space-when-just-before-tab-stop ()
  (should (equal 13 (scoot--visible-width "HELLO	WORLD" 4))))

(ert-deftest scoot--visible-width--tab-width-2--tab-counts-as-2-spaces-when-aligned-with-tab-stop ()
  (should (equal 13 (scoot--visible-width " HELLO	WORLD" 2))))

(ert-deftest scoot--visible-width--tab-width-4--tab-counts-as-4-spaces-when-aligned-with-tab-stop ()
  (should (equal 17 (scoot--visible-width "   HELLO	WORLD" 4))))




;;; scoot-common.visual-width.test.el ends here
