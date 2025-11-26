;;; scoot-widget.management.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-widget)


;; Register Widget

(ert-deftest scoot-widget--register-widget--is-added-to-active-widgets ()
  (let* (;; Given
         (scoot--active-widgets nil)
         ;; When
         (widget (scoot-widget--register-widget! 'query-block 'my-block)))

    ;; Then
    (should widget)
    (should (equal '((query-block--my-block . (:type query-block :name my-block)))
                   scoot--active-widgets))))

(ert-deftest scoot-widget--register-widget--multiple-widgets ()
  (let* (;; Given
         (scoot--active-widgets nil)
         ;; When
         (qb-widget (scoot-widget--register-widget! 'query-block 'my-block))
         (table-widget (scoot-widget--register-widget! 'table 'my-table)))

    ;; Then
    (should qb-widget)
    (should table-widget)
    (should (equal '((table--my-table . (:type table :name my-table))
                     (query-block--my-block . (:type query-block :name my-block)))
                   scoot--active-widgets))))



;; Get Widget

(ert-deftest scoot-widget--get-widget--when-single-widget-exists ()
  (let* (;; Given
         (scoot--active-widgets nil)
         (created-widget (scoot-widget--register-widget! 'query-block 'my-block))
         ;; When
         (widget-by-type-and-name
          (scoot-widget--get-widget :type 'query-block :name 'my-block))
         (widget-by-identity
          (scoot-widget--get-widget :identity 'query-block--my-block)))

    ;; Then
    (should widget-by-type-and-name)
    (should widget-by-identity)
    (should (equal widget-by-type-and-name widget-by-identity))
    (should (equal created-widget widget-by-identity))))


(ert-deftest scoot-widget--get-widget--multiple-widgets ()
  (let* (;;Given
         (scoot--active-widgets nil)
         (qb-widget (scoot-widget--register-widget! 'query-block 'my-block))
         (table-widget (scoot-widget--register-widget! 'table 'my-table))
         ;; When
         (qb-by-type-and-name
          (scoot-widget--get-widget :type 'query-block :name 'my-block))
         (qb-by-identity
          (scoot-widget--get-widget :identity 'query-block--my-block))
         (table-by-type-and-name
          (scoot-widget--get-widget :type 'table :name 'my-table))
         (table-by-identity
          (scoot-widget--get-widget :identity 'table--my-table)))

    ;; Then
    (should qb-by-type-and-name)
    (should qb-by-identity)
    (should table-by-type-and-name)
    (should table-by-identity)

    (should (equal qb-widget qb-by-type-and-name))
    (should (equal qb-widget qb-by-identity))

    (should (equal table-widget table-by-type-and-name))
    (should (equal table-widget table-by-identity))))



;; Set shadow buffer

(ert-deftest scoot-widget--set-shadow-buffer--no-existing-buffer ()
  (let (;;Given
        (widget (scoot-widget--register-widget! 'query-block 'my-block))
        (shadow-buffer (get-buffer-create "*test-buf*")))
    ;; When
    (scoot-widget--set-shadow-buffer! :type 'query-block
                                      :name 'my-block
                                      :buffer shadow-buffer)

    ;; Then
    (should (equal shadow-buffer (plist-get widget :shadow-buffer)))))

(ert-deftest scoot-widget--set-shadow-buffer--existing-buffer ()
  (let (;;Given
        (widget (scoot-widget--register-widget! 'query-block 'my-block))
        (shadow-buffer (get-buffer-create "*test-buf*")))
    (plist-put widget :shadow-buffer "garbage")

    ;; When
    (scoot-widget--set-shadow-buffer! :type 'query-block
                                      :name 'my-block
                                      :buffer shadow-buffer)

    ;; Then
    (should (equal shadow-buffer (plist-get widget :shadow-buffer)))))






;;; scoot-widget.management.test.el ends here
