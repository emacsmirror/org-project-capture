;;; org-category-capture-test.el --- org-category-capture test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2024 Ivan Malison

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The unit test suite of org-category-capture

;;; Code:

(require 'ert)
(require 'noflet)
(require 'dash)

(require 'org-category-capture)
(setq org-adapt-indentation 1)

(defun equal-as-sets (seq1 seq2)
  "Return t if SEQ1 and SEQ2 contain the same elements."
  (and
   (-all? (lambda (element) (member element seq2)) seq1)
   (-all? (lambda (element) (member element seq1)) seq2)))

;; Tests for occ-get-value-by-category

(ert-deftest test-occ-get-value-by-category-handles-missing-categories ()
  "Test that occ-get-value-by-category works with headings lacking CATEGORY."
  (with-temp-buffer
    (org-mode)
    (insert "
* proj2
** TODO do my thing
** TODO cool
* proj1
* proj3
* emacs
  :PROPERTIES:
  :CATEGORY: proj4
  :END:
") (should (equal-as-sets (mapcar 'car (occ-get-value-by-category))
                          '("proj1" "proj2" "proj3" "proj4")))))

(ert-deftest test-occ-get-value-by-category-empty-buffer ()
  "Test occ-get-value-by-category on an empty org buffer."
  (with-temp-buffer
    (org-mode)
    (should (null (occ-get-value-by-category)))))

(ert-deftest test-occ-get-value-by-category-no-headings ()
  "Test occ-get-value-by-category with only text, no headings."
  (with-temp-buffer
    (org-mode)
    (insert "Just some text without any headings.")
    (should (null (occ-get-value-by-category)))))

(ert-deftest test-occ-get-value-by-category-deeply-nested ()
  "Test that only top-level headings are considered as categories."
  (with-temp-buffer
    (org-mode)
    (insert "
* toplevel1
** nested1
*** deeply-nested
* toplevel2
")
    (should (equal-as-sets (mapcar 'car (occ-get-value-by-category))
                           '("toplevel1" "toplevel2")))))

(ert-deftest test-occ-get-value-by-category-with-property-fn ()
  "Test occ-get-value-by-category extracts custom properties."
  (with-temp-buffer
    (org-mode)
    (insert "
* project1
  :PROPERTIES:
  :URGENCY: high
  :END:
* project2
  :PROPERTIES:
  :URGENCY: low
  :END:
")
    (let ((result (occ-get-value-by-category
                   :property-fn (lambda () (org-entry-get (point) "URGENCY")))))
      (should (equal (cdr (assoc "project1" result)) "high"))
      (should (equal (cdr (assoc "project2" result)) "low")))))

;; Tests for occ-get-category-heading-location

(ert-deftest test-get-heading-location-on-empty-file ()
  "Test occ-get-category-heading-location returns nil for empty file."
  (with-temp-buffer
    (org-mode)
    (should (null (occ-get-category-heading-location "test-category")))))

(ert-deftest test-get-heading-location-finds-by-heading-text ()
  "Test finding heading by its text."
  (with-temp-buffer
    (org-mode)
    (insert "
* first
* target
* last
")
    (should (occ-get-category-heading-location "target"))))

(ert-deftest test-get-heading-location-finds-by-category-property ()
  "Test finding heading by CATEGORY property, not heading text."
  (with-temp-buffer
    (org-mode)
    (insert "
* visible-name
  :PROPERTIES:
  :CATEGORY: hidden-category
  :END:
")
    (should (occ-get-category-heading-location "hidden-category"))))

(ert-deftest test-get-heading-location-prefers-category-over-heading ()
  "Test that CATEGORY property takes precedence over heading text."
  (with-temp-buffer
    (org-mode)
    (insert "
* wrong-match
  :PROPERTIES:
  :CATEGORY: target
  :END:
* target
")
    (let ((location (occ-get-category-heading-location "target")))
      (goto-char location)
      (should (string-match-p "wrong-match" (org-get-heading))))))

;; Tests for occ-goto-or-insert-category-heading

(ert-deftest test-goto-or-insert-creates-heading-in-empty-buffer ()
  "Test that new heading is created in empty buffer."
  (with-temp-buffer
    (org-mode)
    (occ-goto-or-insert-category-heading "new-category")
    (should (string-match-p "new-category" (buffer-string)))
    (should (string-match-p ":CATEGORY: new-category" (buffer-string)))))

(ert-deftest test-goto-or-insert-navigates-to-existing ()
  "Test navigation to existing heading without creating duplicate."
  (with-temp-buffer
    (org-mode)
    (insert "
* existing
  :PROPERTIES:
  :CATEGORY: existing
  :END:
")
    (let ((original-content (buffer-string)))
      (goto-char (point-min))
      (occ-goto-or-insert-category-heading "existing")
      (should (equal (buffer-string) original-content)))))

(ert-deftest test-goto-or-insert-with-custom-build-heading ()
  "Test that build-heading function transforms the heading text."
  (with-temp-buffer
    (org-mode)
    (occ-goto-or-insert-category-heading
     "test"
     :build-heading (lambda (h) (concat "PREFIX-" h "-SUFFIX")))
    (should (string-match-p "PREFIX-test-SUFFIX" (buffer-string)))))

;; Tests for occ-level-filter

(ert-deftest test-level-filter-skips-wrong-level ()
  "Test that level filter skips entries at wrong level."
  (with-temp-buffer
    (org-mode)
    (insert "
* level1
** level2
*** level3
")
    (goto-char (point-min))
    (let ((filter (occ-level-filter 1))
          (results nil))
      (org-map-entries
       (lambda ()
         (unless (funcall filter)
           (push (org-get-heading) results))))
      (should (equal (length results) 1))
      (should (string-match-p "level1" (car results))))))

;; Tests for occ-map-entries-for-category

(ert-deftest test-map-entries-for-nonexistent-category ()
  "Test occ-map-entries-for-category returns nil for missing category."
  (with-temp-buffer
    (org-mode)
    (insert "
* existing
** TODO task
")
    (should (null (occ-map-entries-for-category "nonexistent" #'ignore)))))

(ert-deftest test-map-entries-collects-subtree-entries ()
  "Test that all subtree entries are mapped."
  (with-temp-buffer
    (org-mode)
    (insert "
* myproject
** TODO task1
** TODO task2
** DONE task3
* other
** TODO other-task
")
    (let ((headings (occ-map-entries-for-category "myproject" #'org-get-heading)))
      (should (equal (length headings) 4))
      (should (member "myproject" headings)))))

;; Tests for occ-auto-insert-category-heading

(ert-deftest test-auto-insert-category-when-enabled ()
  "Test that CATEGORY is auto-inserted when occ-auto-insert-category-heading is t."
  (with-temp-buffer
    (org-mode)
    (insert "
* no-category-yet
")
    (let ((occ-auto-insert-category-heading t))
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (occ-get-heading-category))
    (should (string-match-p ":CATEGORY: no-category-yet" (buffer-string)))))

(ert-deftest test-no-auto-insert-category-when-disabled ()
  "Test CATEGORY not inserted when occ-auto-insert-category-heading is nil."
  (with-temp-buffer
    (org-mode)
    (insert "
* no-category
")
    (let ((occ-auto-insert-category-heading nil))
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (occ-get-heading-category))
    (should-not (string-match-p ":CATEGORY:" (buffer-string)))))

;; Tests for capture workflow

(defclass occ-test-strategy (occ-strategy) nil
  "Test strategy for unit tests.")

(cl-defmethod occ-get-capture-marker ((_ occ-test-strategy) context)
  "Get marker for test CONTEXT."
  (with-slots (category) context
    (occ-goto-or-insert-category-heading category)
    (point-marker)))

(cl-defmethod occ-target-entry-p ((_ occ-test-strategy) _context)
  "Always target entry for test strategy."
  t)

(defvar occ-test-text-to-insert "dummy-text"
  "Text to insert during test captures.")

(defun occ-do-test-capture (category heading-text)
  "Perform a test capture for CATEGORY with HEADING-TEXT."
  (let ((occ-test-text-to-insert heading-text))
    (occ-capture
     (make-instance 'occ-context :category category :options nil :strategy
                    (make-instance 'occ-test-strategy) :template "* TODO %?\n"))))

(defun occ-mock-place-template (&rest _args)
  "Mock function for org-capture-place-template."
  (goto-char (org-capture-get :pos))
  (setq-local outline-level 'org-outline-level)
  (pcase (org-capture-get :type)
    ((or `nil `entry) (org-capture-place-entry))
    (`table-line (org-capture-place-table-line))
    (`plain (org-capture-place-plain-text))
    (`item (org-capture-place-item))
    (`checkitem (org-capture-place-item)))
  (insert occ-test-text-to-insert))

(ert-deftest test-insert-todo ()
  "Test that TODOs are inserted under correct category headings."
  (with-temp-buffer
    (org-mode)
    (insert "
* cool
* someproj
* anotherproj
  :PROPERTIES:
  :CATEGORY: actualcategory
  :END:
* something
") (noflet ((org-capture-place-template (&rest _args)
                                        (occ-mock-place-template))
            (org-capture-narrow (&rest _args)
                                nil))
     (occ-do-test-capture "someproj" "dummy text")
     (occ-do-test-capture "anotherproj" "dummy text")
     (occ-do-test-capture "actualcategory" "some text")
     (occ-do-test-capture "someproj" "dummy text")
     (should
      (equal (buffer-string) "
* cool
* someproj
** TODO dummy text
** TODO dummy text
* anotherproj
  :PROPERTIES:
  :CATEGORY: actualcategory
  :END:
** TODO some text
* something
* anotherproj
  :PROPERTIES:
  :CATEGORY: anotherproj
  :END:
** TODO dummy text
")))))

(ert-deftest test-capture-to-new-category ()
  "Test capturing to a category that doesn't exist yet."
  (with-temp-buffer
    (org-mode)
    (insert "
* existing
")
    (noflet ((org-capture-place-template (&rest _args)
                                         (occ-mock-place-template))
             (org-capture-narrow (&rest _args)
                                 nil))
      (occ-do-test-capture "brand-new" "first task"))
    (should (string-match-p "brand-new" (buffer-string)))
    (should (string-match-p "first task" (buffer-string)))))

;; Tests for special characters and edge cases

(ert-deftest test-category-with-special-characters ()
  "Test handling of special characters in category names."
  (with-temp-buffer
    (org-mode)
    (insert "
* project-with-dashes
* project_with_underscores
* project.with.dots
")
    (should (occ-get-category-heading-location "project-with-dashes"))
    (should (occ-get-category-heading-location "project_with_underscores"))
    (should (occ-get-category-heading-location "project.with.dots"))))

(ert-deftest test-occ-end-of-properties ()
  "Test occ-end-of-properties positions correctly."
  (with-temp-buffer
    (org-mode)
    (insert "
* heading
  :PROPERTIES:
  :CATEGORY: test
  :CUSTOM: value
  :END:
  Some content here.
")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (occ-end-of-properties)
    (should (looking-at-p "$"))))

(ert-deftest test-occ-insert-subheading ()
  "Test occ-insert-subheading creates proper subheading."
  (with-temp-buffer
    (org-mode)
    (insert "
* parent
  :PROPERTIES:
  :CATEGORY: parent
  :END:
")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (occ-insert-subheading)
    (insert "child")
    (should (string-match-p "\\*\\* child" (buffer-string)))))

(provide 'org-category-capture-test)
;;; org-category-capture-test.el ends here
