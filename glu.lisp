(in-package :glu)

;;; Reader Macros --------------------------------------------------------------
(defvar lambda-symbol-defined nil)
(unless lambda-symbol-defined
  (labels ((λ-reader (stream char)
             "Allow the character 'λ' to be used in place of the word 'lambda',
              for brevity's sake."
             (declare (ignore char stream))
             'LAMBDA))
    (set-macro-character #\λ #'λ-reader)
    (setf lambda-symbol-defined t)))
;;; Reader Macros --------------------------------------------------------------

;;; Taken from PCL -------------------------------------------------------------
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.")

(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))
;;; Taken from PCL -------------------------------------------------------------

;;; Generic Utils --------------------------------------------------------------
(defmacro -> (obj slot)
  "Gets the value of a slot."
  `(slot-value ,obj ',slot))

(defmacro => (obj slot val)
  "Sets the value of a slot."
  `(setf (slot-value ,obj ',slot) ,val))

(defmacro empty? (val)
  "Determine whether 'val' is considered empty. I.e. is an empty sequence
  or an empty string."
  `(or (null ,val)
       (and (listp ,val) (= 0 (length ,val)))
       (and (stringp ,val) (= 0 (length, val)))))

(defmacro sf (control-string &rest args)
  "Convenience macro to format a string. `sf` stands for 'string format'."
  `(format nil ,control-string ,@args))
;;; Generic Utils --------------------------------------------------------------

;;; Logging --------------------------------------------------------------------
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
;;; Logging --------------------------------------------------------------------
