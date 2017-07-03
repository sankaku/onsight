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

;;; onsight.el ends here
