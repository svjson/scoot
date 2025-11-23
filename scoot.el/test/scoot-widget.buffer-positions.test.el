;;; scoot-widget.buffer-positions.test.el --- summary -*- lexical-binding: t -*-

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


;; scoot-widget--point->position

(ert-deftest scoot-widget--point->position--visible-to-shadow ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 3
                             :relative-point 1
                             :expected-pos '(:line 1 :col 0))
                       (list :case "6 chars into first line"
                             :absolute-point 8
                             :relative-point 6
                             :expected-pos '(:line 1 :col 5))
                       (list :case "Start of second line"
                             :absolute-point 53
                             :relative-point 51
                             :expected-pos '(:line 2 :col 0)))))
      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Shadow buffer
          (should (equal (list case-name "Absolute" (plist-get case :expected-pos))
                         (list case-name "Absolute"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :absolute-point)
                                (list :absolute-p t
                                      :shadow-p t)))))

          ;; Relative visible buffer -> Shadow buffer
          (should (equal (list case-name "Relative" (plist-get case :expected-pos))
                         (list case-name "Relative"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :relative-point)
                                (list :shadow-p t))))))))))

(ert-deftest scoot-widget--point->position--visible-to-visible ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 3
                             :absolute-expected-pos '(:line 3 :col 0)
                             :relative-point 1
                             :relative-expected-pos '(:line 1 :col 0))
                       (list :case "6 chars into first line"
                             :absolute-point 8
                             :absolute-expected-pos '(:line 3 :col 5)
                             :relative-point 6
                             :relative-expected-pos '(:line 1 :col 5))
                       (list :case "Start of second line"
                             :absolute-point 53
                             :absolute-expected-pos '(:line 4 :col 0)
                             :relative-point 51
                             :relative-expected-pos '(:line 2 :col 0)))))

      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Absolute visible buffer
          (should (equal (list case-name "Absolute" (plist-get case :absolute-expected-pos))
                         (list case-name "Absolute"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :absolute-point)
                                (list :absolute-p t)))))

          ;; Relative visible buffer -> Relative visible buffer
          (should (equal (list case-name "Relative" (plist-get case :relative-expected-pos))
                         (list case-name "Relative"
                               (scoot-widget--point->position
                                widget
                                (plist-get case :relative-point))))))))))



(ert-deftest scoot-widget--point->position--shadow-to-visible ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :absolute-expected-pos '(:line 3 :col 0)
                             :relative-expected-pos '(:line 1 :col 0))
                       (list :case "6 chars into first line"
                             :shadow-point 6
                             :absolute-expected-pos '(:line 3 :col 5)
                             :relative-expected-pos '(:line 1 :col 5))
                       (list :case "Start of second line"
                             :shadow-point 8
                             :absolute-expected-pos '(:line 4 :col 0)
                             :relative-expected-pos '(:line 2 :col 0)))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Absolute visible buffer
            (should (equal (list case-name "Absolute"
                                 (plist-get case :absolute-expected-pos))
                           (list case-name "Absolute"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :absolute-p t)))))

            ;; Shadow buffer -> Relative visible buffer
            (should (equal (list case-name "Relative"
                                 (plist-get case :relative-expected-pos))
                           (list case-name "Relative"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)))))))))))

(ert-deftest scoot-widget--point->position--shadow-to-shadow ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :expected-pos '(:line 1 :col 0))
                       (list :case "6 chars into first line"
                             :shadow-point 6
                             :expected-pos '(:line 1 :col 5))
                       (list :case "Start of second line"
                             :shadow-point 8
                             :expected-pos '(:line 2 :col 0)))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            ;; Shadow buffer -> Shadow buffer (absolute has no effect)
            (should (equal (list case-name "(Absolute*)" (plist-get case :expected-pos))
                           (list case-name "(Absolute*)"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :absolute-p t
                                        :shadow-p t)))))

            ;; Shadow buffer -> Shadow buffer
            (should (equal (list case-name "(Relative*)" (plist-get case :expected-pos))
                           (list case-name "(Relative*)"
                                 (scoot-widget--point->position
                                  widget
                                  (plist-get case :shadow-point)
                                  (list :shadow-p t)))))))))))



;; scoot-widget--point->point

(ert-deftest scoot-widget--point->point--visible-to-shadow ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 3
                             :relative-point 1
                             :expected-point 1)
                       (list :case "6 chars into first line"
                             :absolute-point 8
                             :relative-point 6
                             :expected-point 6)
                       (list :case "Start of second line"
                             :absolute-point 53
                             :relative-point 51
                             :expected-point 8))))
      (dolist (case cases)
        (let ((case-name (plist-get case :case)))
          ;; Absolute visible buffer -> Shadow buffer
          (should (equal (list case-name "Absolute" (plist-get case :expected-point))
                         (list case-name "Absolute"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :absolute-point)
                                (list :from-absolute-p t
                                      :shadow-p t)))))

          ;; Relative visible buffer -> Shadow buffer
          (should (equal (list case-name "Relative" (plist-get case :expected-point))
                         (list case-name "Relative"
                               (scoot-widget--point->point
                                widget
                                (plist-get case :relative-point)
                                (list :shadow-p t))))))))))

(ert-deftest scoot-widget--point->point--visible-to-visible ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Widget Start"
                             :absolute-point 3
                             :relative-point 1)
                       (list :case "6 chars into first line"
                             :absolute-point 8
                             :relative-point 6)
                       (list :case "Start of second line"
                             :absolute-point 53
                             :relative-point 51))))

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



(ert-deftest scoot-widget--point->point--shadow-to-visible ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1
                             :absolute-point 3
                             :relative-point 1)
                       (list :case "6 chars into first line"
                             :shadow-point 6
                             :absolute-point 8
                             :relative-point 6)
                       (list :case "Start of second line"
                             :shadow-point 8
                             :absolute-point 53
                             :relative-point 51))))
      (with-scoot-widget-shadow-buffer widget
        (dolist (case cases)
          (let ((case-name (plist-get case :case)))
            (message "%s - %s" case-name (plist-get widget :widget-start-line))
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

(ert-deftest scoot-widget--point->point--shadow-to-shadow ()
  (with-temp-buffer
    (insert "\n\n")
    (let ((widget (scoot-qb--insert-query-block "SELECT\n  *\nFROM\n  somewhere" '(:width 50)))
          (cases (list (list :case "At Content Start"
                             :shadow-point 1)
                       (list :case "6 chars into first line"
                             :shadow-point 6)
                       (list :case "Start of second line"
                             :shadow-point 8))))
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




;;; scoot-widget.buffer-positions.test.el ends here
