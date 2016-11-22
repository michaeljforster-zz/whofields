# whofields

Whofields is an HTML field rendering and input validation utility
library written in Common Lisp.

Whofields depends on [CL-WHO](http://www.weitz.de/cl-who/). Whofields
is being developed
with [SBCL](http://sbcl.org/), [CCL](http://ccl.clozure.com/),
and [LispWorks](http://www.lispworks.com/) on OS X.  Whofields is
being deployed with SBCL on Linux/AMD64.


## Field Specifiers

Whofields defines the notion of a field specifier--a *fieldspec*--to
specify the name, label, and control-specific properties of a
field. Whofields currently supports :TEXT, :PASSWORD, :SELECT, :RADIO,
and :CHECKBOX fieldspecs.

Whofields provides functions for rendering HTML form controls and for
validating HTTP POST and GET parameter values according to
fieldspecs. However, Whofields makes no assumptions about the means of
obtaining HTTP parameters; Whofields accepts either single values or
alists of parameter names and values.


### Installation

```lisp
(ql:quickload "whofields")
```

### Example

```lisp

(defun parse-string (x)
  (if (stringp x)
      (values (string-trim '(#\Space #\Tab #\Newline) x) t)
      (values nil nil)))

(defun parse-non-empty-string (x)
  (if (stringp x)
      (let ((x (string-trim '(#\Space #\Tab #\Newline) x)))
        (if (string/= "" x)
            (values x t)
            (values nil nil)))
      (values nil nil)))

(defparameter *provinces*
  '(("AB" . "Alberta)
    ("BC" . "British Columbia")
    ("MB" . "Manitoba")
    ("NB" . "New Brunswick")
    ("NL" . "Newfoundland and Labrador")
    ("NT" . "Northwest Territories")
    ("NS" . "Nova Scotia")
    ("NU" . "Nunavut")
    ("ON" . "Ontario")
    ("PE" . "Prince Edward Island")
    ("QC" . "Quebec")
    ("SK" . "Saskatchewan")
    ("YT" . "Yukon")))

(defparameter *registration-fieldspecs*
  (list (list 'username :text :label "Corporation" :validation-function #'parse-non-empty-string :placeholder "Corporation")
        (list 'full-name :text :label "Full Name" :validation-function #'parse-non-empty-string :placeholder "Full Name")
        (list 'email :text :label "Email" :validation-function #'parse-non-empty-string :placeholder "Email")
        (list 'email2 :text :label "Email (confirm)" :validation-function #'parse-non-empty-string :placeholder "Email (confirm)")
        (list 'phone :text :label "Phone" :validation-function #'parse-non-empty-string :placeholder "Phone")
        (list 'alt-phone :text :label "Alt. Phone" :validation-function #'parse-string :placeholder "Alt. Phone")
        (list 'address :text :label "Address" :validation-function #'parse-non-empty-string :placeholder "Address")
        (list 'city :text :label "City" :validation-function #'parse-non-empty-string :placeholder "City")
        (list 'province :select :label "Province"
              :options *provinces*
              :key #'car
              :label-function #'cdr
              :test #'string=
              :value "")
        (list 'postal-code :text :label "Postal Code" :validation-function #'parse-non-empty-string :placeholder "Postal Code")
        (list 'agrees-to-mailing-list :label "I want to join the mailing list" :checkbox :truep t)))

(defparameter *register-fieldvals*
  (list (cons 'username "")
        (cons 'full-name "")
        (cons 'email "")
        (cons 'email2 "")
        (cons 'phone "")
        (cons 'alt-phone "")
        (cons 'address "")
        (cons 'city "")
        (cons 'province "ON")
        (cons 'postal-code "")
        (cons 'agrees-to-mailing-list nil)))

(hunchentoot:define-easy-handler (handle-registration :uri (princ-to-string "/registration"))
    ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (cond ((equal (hunchentoot:request-method*) :post)
         (multiple-value-bind (fieldvals fielderrs)
             (hunchentools:validate-fields *registration-fieldspecs*
                                           (hunchentoot:post-parameters*))
           (unless (string= (hunchentoot:post-parameter "email")
                            (hunchentoot:post-parameter "email2"))
             (push 'email2 fielderrs))
           (if fielderrs
               (render-registration-page fieldvals fielderrs)
               (progn
                 (model:register fieldvals)
                 (hunchentoot:redirect "/registration-complete")))))
        ((equal (hunchentoot:request-method*) :get)
         (render-registration-page *registration-fieldvals* '()))))
```

### License

Whofields is distributed under the MIT license. See LICENSE.
