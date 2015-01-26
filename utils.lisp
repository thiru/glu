;; Allow the character "λ" to be used in place of the word "lambda" for brevity's sake.
;; TODO: Find a way to ensure that this only gets called once...
(defun λ-reader (stream char)
  (declare (ignore char stream))
  'LAMBDA)
(set-macro-character #\λ #'λ-reader)

;; A control string to print a list of items in a friendly manner.
;; E.g. "1 and 2", or "1, 2 and 3".
;; This was taken from PCL.
(defparameter *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}")
