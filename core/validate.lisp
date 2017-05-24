;;;; core/validate.lisp

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

(defpackage "WHOFIELDS/CORE/VALIDATE"
  (:use "CL")
  (:import-from "WHOFIELDS/CORE/FIELDSPEC")
  (:export "VALIDATE-FIELD"
           "VALIDATE-FIELDS"))

(in-package "WHOFIELDS/CORE/VALIDATE")

(defun validate-string (value &key validation-function &allow-other-keys)
  (funcall validation-function value))

(defun validate-option (value
                        &key options
                          (test #'eql)
                          (key #'identity)
                          &allow-other-keys)
  (let ((option (find value options :key key :test test)))
    (if option
        (values (funcall key option) t)
        (values nil nil))))

(defun validate-boolean (value &key truep &allow-other-keys)
  (cond ((and truep (not (null value)))
         (values t t))
        ((and truep (null value))
         (values nil nil))
        (t
         (values (not (null value)) t))))

(defun validate-field (fieldspec value)
  (destructuring-bind (name type &rest rest-args)
      fieldspec
    (declare (ignore name))
    (ecase type
      (:text (apply #'validate-string value rest-args))
      (:password (apply #'validate-string value rest-args))
      (:select (apply #'validate-option value rest-args))
      (:radio (apply #'validate-option value rest-args))
      (:checkbox (apply #'validate-boolean value rest-args)))))

(defun validate-fields (fieldspecs fieldvals)
  (flet ((fieldval (name)
           (cdr (assoc (string-downcase name)
                       fieldvals
                       :test #'string=))))
    (let ((vals '())
          (errs '()))
      (dolist (fieldspec fieldspecs)
        (let ((name (whofields/core/fieldspec:fieldspec-name fieldspec)))
          (multiple-value-bind (val success-p)
              (validate-field fieldspec (fieldval name))
            (if success-p
                (push (cons name val) vals)
                (push name errs)))))
      (values vals errs))))
