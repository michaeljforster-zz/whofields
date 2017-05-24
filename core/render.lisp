;;;; core/render.lisp

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

(defpackage "WHOFIELDS/CORE/RENDER"
  (:use "CL")
  (:import-from "CL-WHO")
  (:export "RENDER-FIELD"))

(in-package "WHOFIELDS/CORE/RENDER")

(defun escape-value (value)
  (if (null value)
      ""
      (cl-who:escape-string (princ-to-string value))))

(defun render-text-input (stream
                          name
                          value
                          &key (placeholder "")
                            &allow-other-keys)
  (cl-who:with-html-output (stream)
    (:input :class "form-control"
            :type "text"
            :id (cl-who:escape-string name)
            :name (cl-who:escape-string name)
            :placeholder (cl-who:escape-string placeholder)
            :value (escape-value value))))

(defun render-password-input (stream
                              name
                              value
                              &key (placeholder "")
                                &allow-other-keys)
  (cl-who:with-html-output (stream)
    (:input :class "form-control"
            :type "password"
            :id (cl-who:escape-string name)
            :name (cl-who:escape-string name)
            :placeholder (cl-who:escape-string placeholder)
            :value (escape-value value))))

(defun render-select (stream
                      name
                      value
                      &key options
                        (test #'eql)
                        (key #'identity)
                        (label-function #'identity)
                        &allow-other-keys)
  (cl-who:with-html-output (stream)
    (:select :class "form-control"
             :id (cl-who:escape-string name)
             :name (cl-who:escape-string name)
             (dolist (option options)
               (let ((option-value (funcall key option))
                     (option-label (funcall label-function option)))
                 (cl-who:htm
                  (:option :value (escape-value option-value)
                           :selected (funcall test option-value value)
                           (cl-who:esc option-label))))))))

(defun render-checkbox (stream
                        name
                        value
                        &key &allow-other-keys)
  (cl-who:with-html-output (stream)
    (:input :type "checkbox" :name (cl-who:escape-string name) :value "t"
            :checked (not (null value)))))

(defun render-field (stream fieldspec value)
  (destructuring-bind (name type &rest rest-args)
      fieldspec
    (ecase type
      (:text (apply #'render-text-input stream (string-downcase name) value rest-args))
      (:password (apply #'render-password-input stream (string-downcase name) value rest-args))
      (:select (apply #'render-select stream (string-downcase name) value rest-args))
      (:checkbox (apply #'render-checkbox stream (string-downcase name) value rest-args)))))
