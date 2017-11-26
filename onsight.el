;;; onsight.el --- Extreme speed in Emacs

;; Copyright (C) 2017 sankaku

;; Author: sankaku <sankaku.git@gmail.com>
;; Version: 0.01
;; Package-Requires: ((emacs "26.0"))
;; URL: https://github.com/sankaku/onsight

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(require 'cl)

(setq onsight--divide-array '((2 2) (2 2) (2 2)))
(defvar onsight--ovrs-list nil)


(defun onsight--calc-division (left-top right-bottom vertical-division horizontal-division)
  "Divide the rectangle.
  left-top and right-bottom are both cons cells.
  ((1 . 1) (25 . 30) 2 3) -> ((1 . 1) (1 . 10) (1 . 19) (1 . 30) (13 . 1) (13 . 10) (13 . 19) (13 . 30) (25 . 1) (25 . 10) (25 . 19) (25 . 30))"
  (let ((r0 (car left-top))
        (c0 (cdr left-top))
        (r1 (car right-bottom))
        (c1 (cdr right-bottom)))
    (onsight--zip-all-combi
           (onsight--simple-split r0 r1 (onsight--simple-divide r0 r1 vertical-division) nil)
           (onsight--simple-split c0 c1 (onsight--simple-divide c0 c1 horizontal-division) nil))))


(defun onsight--simple-divide (from to division)
  "(1 20 3) -> 6"
  (floor (/ (- to from) division)))

(defun onsight--move-to-rc (row column)
  "Move cursor to (row . column)."
  (move-to-window-line row)
  (move-to-column column))

(defun onsight--zip-all-combi (list1 list2)
  "((a b) (A B C)) -> ((a . A) (a . B) (a . C) (b . A) ( b . B) (b . C))"
  (reduce #'append (mapcar (lambda (x)(mapcar (lambda (y) (cons x y)) list2)) list1)))

(defun onsight--simple-split (from to width l)
  "(1 20 6 nil) -> (1 7 13 20)"
  (if (< to (+ from width))
      (append l (list to))
    (onsight--simple-split (+ from width) to width (append l (list from)))))

(defun onsight--make-add-string (line-end-column bgn-column end-column face)
  "Returns string with face for adding after `line-end-column`.
   If `line-end-column` is less than `bgn-column`
   this returns transparent blanks (for adding before `bgn-column`) and colored blanks with `face` (for adding between `bgn-column` and `end-column`).
   If it is between `bgn-column` and `end-column` this returns colored blanks between `line-end-column` and `end-column`.
   If it is bigger than `end-column` this returns empty string."
  (let* ((transparent-blanks (make-string (max 0 (- bgn-column line-end-column)) ? ))
         (colored-blanks (propertize (make-string (max 0 (- end-column (max bgn-column line-end-column))) ? ) 'face face)))
    (concat transparent-blanks colored-blanks)))

(defun onsight--make-ovrs (line-end-column bgn-column end-column)
  "Returns a cons cell of two overlays for the current line:
   to setting background color for existing text and to adding string for end of line."
  (let* ((before-p (< line-end-column bgn-column))
         (after-p (> line-end-column end-column))
         (line-end-point (save-excursion (onsight--move-to-column line-end-column) (point)))
         (bgn-point (save-excursion (onsight--move-to-column bgn-column) (point)))
         (end-point (save-excursion (onsight--move-to-column end-column) (point)))
         (text-ovr (cond (before-p
                          (make-overlay line-end-point line-end-point))
                         (after-p
                          (make-overlay bgn-point end-point))
                         (t
                          (make-overlay bgn-point line-end-point))))
         (add-string-ovr (make-overlay line-end-point line-end-point)))
    `(,text-ovr . ,add-string-ovr)))

(defun onsight--move-to-column (column)
  (vertical-motion `(,column . 0)))

(defun onsight--get-line-end-column ()
  "TODO truncated"
  (save-excursion
    (vertical-motion `(,(1- (window-body-width)) . 0 ))
    (current-column)))

(defun onsight--put-ovrs (bgn-column end-column face)
  "Put overlays between `bgn-column` and `end-column` of the current line with `face`."
  (let* ((line-end-column (onsight--get-line-end-column))
         (ovrs (onsight--make-ovrs line-end-column bgn-column end-column))
         (text-ovr (car ovrs))
         (add-string-ovr (cdr ovrs))
         (add-string (onsight--make-add-string line-end-column bgn-column end-column face)))
    (overlay-put text-ovr 'face face)
    (overlay-put add-string-ovr 'after-string add-string)
    (push text-ovr onsight--ovrs-list)
    (push add-string-ovr onsight--ovrs-list)))

(defun onsight--put-ovrs-rectangle (left-top right-bottom face)
  "Put overlays for the rectangle.
   The vertices of the rectangle, `left-top` and `right-bottom`, are both cons cells."
  (let* ((bgn-line (car left-top))
         (end-line (car right-bottom))
         (lines (number-sequence bgn-line end-line))
         (bgn-column (cdr left-top))
         (end-column (cdr right-bottom)))
    (save-excursion
      (dolist (line lines)
        (move-to-window-line line)
        (onsight--put-ovrs bgn-column end-column face)))))

;(onsight--put-ovrs-rectangle '(10 . 10) '(15 . 80) '(t :background "red"))
;(onsight--put-ovrs-rectangle '(16 . 20) '(35 . 70) '(t :background "blue"))

(defun onsight--clear-ovrs ()
  "Delete all overlays."
  (interactive)
  (dotimes (x (length onsight--ovrs-list))
    (delete-overlay (pop onsight--ovrs-list))))

;;; onsight.el ends here
