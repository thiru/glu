;;;; The following functions are taken directly from Doug Hoyte's Let Over
;;;; Lambda, or are minor modifications there of.

(in-package :glu)

;; (Daniel Herring's nestable suggestion)
(defun sharp-double-quote-reader (stream sub-char numarg)
  "Facilitates creating strings that have double-quote and backslashes
   without escaping."
  (declare (ignore sub-char numarg))
  (let (chars (state 'normal) (depth 1))
    (loop do
      (let ((curr (read-char stream)))
        (cond ((eq state 'normal)
                 (cond ((char= curr #\#)
                          (push #\# chars)
                          (setq state 'read-sharp))
                       ((char= curr #\")
                          (setq state 'read-quote))
                       (t
                          (push curr chars))))
              ((eq state 'read-sharp)
                 (cond ((char= curr #\")
                          (push #\" chars)
                          (incf depth)
                          (setq state 'normal))
                       (t
                          (push curr chars)
                          (setq state 'normal))))
              ((eq state 'read-quote)
                 (cond ((char= curr #\#)
                          (decf depth)
                          (if (zerop depth) (return))
                          (push #\" chars)
                          (push #\# chars)
                          (setq state 'normal))
                       (t
                          (push #\" chars)
                          (if (char= curr #\")
                            (setq state 'read-quote)
                            (progn
                              (push curr chars)
                              (setq state 'normal)))))))))
   (coerce (nreverse chars) 'string)))
(set-dispatch-macro-character
  #\# #\" #'sharp-double-quote-reader)

;; (Martin Dirichs' version)
(defun sharp-greater-than-reader (stream sub-char numarg)
  "Facilitates creating strings with a user-defined terminator.
   For example:
     #>END
     Any characters can be placed here. The only thing that will terminate the
     reading of this string is...END"
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let ((pattern (nreverse chars))
          output)
      (labels ((match (pos chars)
        (if (null chars)
          pos
          (if (char= (nth pos pattern) (car chars))
              (match (1+ pos) (cdr chars))
              (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
        (do (curr
             (pos 0))
            ((= pos (length pattern)))
          (setf curr (read-char stream)
                pos (match pos (list curr)))
          (push curr output))
        (coerce
          (nreverse
            (nthcdr (length pattern) output))
          'string)))))
(set-dispatch-macro-character
  #\# #\> #'sharp-greater-than-reader)

(defmacro dlambda (&rest ds)
  "The d in dlambda stands for dispatching or destructuring.
   A closure can be used a simpler alternative to an object. However, an
   object with only a single method. dlambda supports the possibility of
   specifying multiple methods.
   dlambda expands into a lambda that can be destructured into executing
   different 'methods' depending on the keyword arguments given to it.
   For example:
   (setf (symbol-function 'count-test)
     (let ((count 0))
       (dlambda
         (:reset () (setf count 0)) ; Method with no args
         (:inc (n) (incf count n)) ; Method with 1 arg
         (:dec (n) (decf count n))
         ;; The following will be called when no keyword arguments are
         ;; specified. This could be a good place to have a textual
         ;; representation of the object.
         (t ()
            (format nil \"COUNT: ~a\" count)))))"
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
             (lambda (d)
               `(,(if (eq t (car d))
                    t
                    (list (car d)))
                 (apply (lambda ,@(cdr d))
                        ,(if (eq t (car d))
                           args
                           `(cdr ,args)))))
             ds)))))
