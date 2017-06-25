;;; onsight.el --- Extreme speed in Emacs

;; Copyright (C) 2017 sankaku

;; Author: sankaku <sankaku.git@gmail.com>
;; Version: 0.01
;; Package-Requires: ((emacs "26.0"))
;; URL: https://github.com/sankaku/onsight

;; MIT License
;; 
;; Copyright (c) 2017 sankaku
;; 
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

(setq onsight--divide-array '((2 2) (2 2) (2 2)))

(defun onsight--calc-division (left-up right-bottom vertical-division horizontal-division)
  "Divide the rectangle.
  left-up and right-down are both cons cells."
  (list (onsight--simple-divide (car left-up) (car right-bottom) vertical-division)
        (onsight--simple-divide (cdr left-up) (cdr right-bottom) horizontal-division)))

(defun onsight--simple-divide (from to division)
  (floor (/ (- to from) division)))

(defun onsight--move-to-rc (row column)
  "Move cursor to (row . column)."
  (move-to-window-line row)
  (move-to-column column))

;;; onsight.el ends here
