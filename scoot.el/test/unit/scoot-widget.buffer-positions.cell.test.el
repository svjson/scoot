;;; scoot-widget.buffer-positions.cell.test.el --- summary -*- lexical-binding: t -*-

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
(require 'scoot-query-block)
(require 'scoot-test-fixtures)


;; scoot-widget--point->position

(ert-deftest scoot-widget--point->position--cell-visible-to-shadow ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "test")
                                              :type 'string))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 110
                             :relative-point 1
                             :expected-pos '(:line 1 :col 0))
                       (list :case "2 chars into input"
                             :absolute-point 112
                             :relative-point 3
                             :expected-pos '(:line 1 :col 2))
                       (list :case "At Widget End"
                             :absolute-point 119
                             :relative-point 10
                             :expected-pos '(:line 1 :col 9)))))
      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Shadow buffer
          (should (equal (list case-name "Absolute"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :absolute-point)
                                (list :absolute-p t
                                      :shadow-p t)))
                         (list case-name "Absolute" (plist-get case :expected-pos))))

          ;; Relative visible buffer -> Shadow buffer
          (should (equal (list case-name "Relative" (plist-get case :expected-pos))
                         (list case-name "Relative"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :relative-point)
                                (list :shadow-p t))))))))))

(ert-deftest scoot-widget--point->position--cell-visible-to-visible ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "test")
                                              :type 'string))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 110
                             :absolute-expected-pos '(:line 3 :col 9)
                             :relative-point 1
                             :relative-expected-pos '(:line 1 :col 0))
                       (list :case "2 chars into input"
                             :absolute-point 112
                             :absolute-expected-pos '(:line 3 :col 11)
                             :relative-point 3
                             :relative-expected-pos '(:line 1 :col 2))
                       (list :case "At Widget End"
                             :absolute-point 119
                             :absolute-expected-pos '(:line 3 :col 18)
                             :relative-point 10
                             :relative-expected-pos '(:line 1 :col 9)))))

      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Absolute visible buffer
          (should (equal (list case-name "Absolute"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :absolute-point)
                                (list :absolute-p t)))
                         (list case-name
                               "Absolute"
                               (plist-get case :absolute-expected-pos))))

          ;; Relative visible buffer -> Relative visible buffer
          (should (equal (list case-name "Relative"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :relative-point)))
                         (list case-name "Relative"
                               (plist-get case :relative-expected-pos)))))))))



(ert-deftest scoot-widget--point->position--cell-shadow-to-visible ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :absolute-expected-pos '(:line 3 :col 9)
                             :relative-expected-pos '(:line 1 :col 0))
                       (list :case "2 chars into input"
                             :shadow-point 3
                             :absolute-expected-pos '(:line 3 :col 11)
                             :relative-expected-pos '(:line 1 :col 2))
                       (list :case "At Content End"
                             :shadow-point 10
                             :absolute-expected-pos '(:line 3 :col 18)
                             :relative-expected-pos '(:line 1 :col 9)))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Absolute visible buffer
            (should (equal (list case-name "Absolute"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :absolute-p t)))
                           (list case-name "Absolute"
                                 (plist-get case :absolute-expected-pos))))

            ;; Shadow buffer -> Relative visible buffer
            (should (equal (list case-name "Relative"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)))
                           (list case-name "Relative"
                                 (plist-get case :relative-expected-pos))))))))))

(ert-deftest scoot-widget--point->position--cell-shadow-to-shadow ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :expected-pos '(:line 1 :col 0))
                       (list :case "6 chars into first line"
                             :shadow-point 6
                             :expected-pos '(:line 1 :col 5))
                       (list :case "At Content End"
                             :shadow-point 10
                             :expected-pos '(:line 1 :col 9)))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Shadow buffer (absolute has no effect)
            (should (equal (list case-name "(Absolute*)"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :absolute-p t
                                        :shadow-p t)))
                           (list case-name "(Absolute*)"
                                 (plist-get case :expected-pos))))

            ;; Shadow buffer -> Shadow buffer
            (should (equal (list case-name "(Relative*)"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :shadow-p t)))
                           (list case-name "(Relative*)"
                                 (plist-get case :expected-pos))))))))))



;; scoot-widget--point->point

(ert-deftest scoot-widget--point->point--cell-visible-to-shadow ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 110
                             :relative-point 1
                             :expected-point 1)
                       (list :case "6 chars into input"
                             :absolute-point 115
                             :relative-point 6
                             :expected-point 6)
                       (list :case "At Widget End"
                             :absolute-point 119
                             :relative-point 10
                             :expected-point 10))))
      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Shadow buffer
          (should (equal (list case-name "Absolute"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :absolute-point)
                                (list :from-absolute-p t
                                      :shadow-p t)))
                         (list case-name "Absolute"
                               (plist-get case :expected-point))))

          ;; Relative visible buffer -> Shadow buffer
          (should (equal (list case-name "Relative"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :relative-point)
                                (list :shadow-p t)))
                         (list case-name "Relative"
                               (plist-get case :expected-point)))))))))

(ert-deftest scoot-widget--point->point--cell-visible-to-visible ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 110
                             :relative-point 1)
                       (list :case "6 chars into first line"
                             :absolute-point 115
                             :relative-point 6)
                       (list :case "At Widget End"
                             :absolute-point 119
                             :relative-point 10))))

      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Absolute visible buffer
          (should (equal (list case-name "Absolute->Absolute"
                               (plist-get case :absolute-point))
                         (list case-name "Absolute->Absolute"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :absolute-point)
                                (list :from-absolute-p t
                                      :to-absolute-p t)))))

          ;; Absolute visible buffer -> Relative visible buffer
          (should (equal (list case-name "Absolute->Relative"
                               (plist-get case :relative-point))
                         (list case-name "Absolute->Relative"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :absolute-point)
                                (list :from-absolute-p t)))))

          ;; Relative visible buffer -> Absolute visible buffer
          (should (equal (list case-name "Relative->Absolute"
                               (plist-get case :absolute-point))
                         (list case-name "Relative->Absolute"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :relative-point)
                                (list :to-absolute-p t)))))

          ;; Relative visible buffer -> Relative visible buffer
          (should (equal (list case-name "Relative->Relative"
                               (plist-get case :relative-point))
                         (list case-name "Relative->Relative"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :relative-point))))))))))



(ert-deftest scoot-widget--point->point--cell-shadow-to-visible ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :absolute-point 110
                             :relative-point 1)
                       (list :case "6 chars into input"
                             :shadow-point 6
                             :absolute-point 115
                             :relative-point 6)
                       (list :case "At Content End"
                             :shadow-point 10
                             :absolute-point 119
                             :relative-point 10))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Absolute visible buffer
            (should (equal (list case-name "Absolute"
                                 (plist-get case :absolute-point))
                           (list case-name "Absolute"
                                 (scoot-widget--point->point
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :to-absolute-p t)))))

            ;; Shadow buffer -> Relative visible buffer
            (should (equal (list case-name "Relative"
                                 (plist-get case :relative-point))
                           (list case-name "Relative"
                                 (scoot-widget--point->point
                                  widget
                                  (plist-get case :shadow-point)))))))))))

(ert-deftest scoot-widget--point->point--cell-shadow-to-shadow ()
  (with-temp-buffer
    (insert-whitespace-block 49 10)
    (let ((widget (scoot-input--install-input :begin 110 :end 120
                                              :formatter scoot-formatter-string
                                              :record-cell (list :formatted-value "full_input")
                                              :type 'string))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1)
                       (list :case "4 chars into input"
                             :shadow-point 4)
                       (list :case "At Content End"
                             :shadow-point 10))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Shadow buffer (absolute has no effect)
            (should (equal (list case-name "(Absolute*)" (plist-get case :shadow-point))
                           (list case-name "(Absolute*)"
                                 (scoot-widget--point->point
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :absolute-p t
                                        :shadow-p t)))))

            ;; Shadow buffer -> Shadow buffer
            (should (equal (list case-name "(Relative*)" (plist-get case :shadow-point))
                           (list case-name "(Relative*)"
                                 (scoot-widget--point->point
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :shadow-p t)))))))))))




;;; scoot-widget.buffer-positions.cell.test.el ends here
