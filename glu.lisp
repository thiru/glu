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
   E.g. '1 and 2', or '1, 2 and 3'.")

;; Taken from PCL
(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))

(defmacro -> (obj slot)
  "Gets the value of a slot."
  `(slot-value ,obj ',slot))

(defmacro => (obj slot val)
  "Sets the value of a slot."
  `(setf (,(symb (type-of (symbol-value obj)) "-" slot) ,obj) ,val))

(defmacro fmt (control-string &rest args)
  "Convenience macro to format a string."
  `(format nil ,control-string ,@args))

;;; Logging:

(defvar *log-format-time*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\. :msec
    #\space :timezone)
  "This is used by `logm` as the date/time format.")

(defun logm (level msg &rest msg-args)
  "Logs a message to the console."
  (format t "* ~A~%  * ~A~%  * ~A~%"
          (format-timestring nil (now) :format *log-format-time*)
          level
          (apply #'format nil msg msg-args)))
