;;;; test/setup.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2016 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage "WHOFIELDS/TEST/SETUP"
  (:use "CL")
  (:export "MY-STRINGP"
           "TEXT-FIELDSPEC"
           "PASSWORD-FIELDSPEC"
           "SELECT-FIELDSPEC"
           "RADIO-FIELDSPEC"
           "CHECKBOX-FIELDSPEC"))

(in-package "WHOFIELDS/TEST/SETUP")

(defun my-stringp (x)
  (if (stringp x)
      (values x t)
      (values nil nil)))

(defun text-fieldspec ()
  (list 'my-text :text :validation-function #'my-stringp :label "MY-TEXT" :placeholder "MY-TEXT" :help-text "Enter MY-TEXT" :value ""))

(defun password-fieldspec ()
  (list 'my-password :password :validation-function #'my-stringp :label "MY-PASSWORD" :placeholder "MY-PASSWORD" :help-password "Enter MY-PASSWORD" :value ""))

(defun select-fieldspec ()
  (list 'my-select :select :options '((1 . "One") (2 . "Two") (3 . "Three")) :test #'eql :key #'car :label-function #'cdr :value 1))

(defun radio-fieldspec ()
  (list 'my-radio :radio :options '((1 . "One") (2 . "Two") (3 . "Three")) :test #'eql :key #'car :label-function #'cdr :value 1))

(defun checkbox-fieldspec (truep)
  (list 'my-checkbox :checkbox :truep truep))
