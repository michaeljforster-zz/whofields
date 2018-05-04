;;;; test/validate.lisp

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

(defpackage "WHOFIELDS/TEST/VALIDATE"
  (:use "CL"
        "LISP-UNIT")
  (:import-from "WHOFIELDS/CORE/VALIDATE")
  (:import-from "WHOFIELDS/TEST/SETUP")
  (:export "VALIDATE-TEXT-FIELD"
           "VALIDATE-PASSWORD-FIELD"
           "VALIDATE-SELECT-FIELD"
           "VALIDATE-RADIO-FIELD"
           "VALIDATE-CHECKBOX-FIELD"))

(define-test validate-text-field
    (assert-equal
     '("FOO" t)
     (multiple-value-list
      (whofields/core/validate:validate-field
       (whofields/test/setup:text-fieldspec) "FOO")))
  (assert-equal
   '(nil nil)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:text-fieldspec) 'foo))))

(define-test validate-password-field
    (assert-equal
     '("FOO" t)
     (multiple-value-list
      (whofields/core/validate:validate-field
       (whofields/test/setup:password-fieldspec) "FOO")))
  (assert-equal
   '(nil nil)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:password-fieldspec) 'foo))))

(define-test validate-select-field
    (assert-equal
     '(2 t)
     (multiple-value-list
      (whofields/core/validate:validate-field
       (whofields/test/setup:select-fieldspec) 2)))
  (assert-equal
   '(nil nil)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:select-fieldspec) 666))))

(define-test validate-radio-field
    (assert-equal
     '(3 t)
     (multiple-value-list
      (whofields/core/validate:validate-field
       (whofields/test/setup:radio-fieldspec) 3)))
  (assert-equal
   '(nil nil)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:radio-fieldspec) 666))))

(define-test validate-checkbox-field
    (assert-equal
     '(t t)
     (multiple-value-list
      (whofields/core/validate:validate-field
       (whofields/test/setup:checkbox-fieldspec t) t)))
  (assert-equal
   '(nil nil)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:checkbox-fieldspec t) nil)))
  (assert-equal
   '(t t)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:checkbox-fieldspec nil) t)))
  (assert-equal
   '(nil t)
   (multiple-value-list
    (whofields/core/validate:validate-field
     (whofields/test/setup:checkbox-fieldspec nil) nil))))
