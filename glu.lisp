(in-package :glu)

(defvar lambda-symbol-defined nil)
(unless lambda-symbol-defined
  (labels ((λ-reader (stream char)
             "Allow the character 'λ' to be used in place of the word 'lambda',
              for brevity's sake."
             (declare (ignore char stream))
             'LAMBDA))
    (set-macro-character #\λ #'λ-reader)
    (setf lambda-symbol-defined t)))

;; Taken from PCL
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.
   This was taken from PCL.")

;; Taken from PCL
(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))
