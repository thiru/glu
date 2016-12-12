(in-package :glu)

#||

* Observer:
  * next (value)
  * error (error)
  * complete ()
* Observable:
  * subscribe (observer)
    * returns unsubscribable
* Operator:
  * takes observable
  * returns observable

* Sample call:
  `(rx-subscribe (rx-map (rx-range 3 8) #'1+) rx-print-observer)`

* Observer will no longer pass values after:
  * (error) is called
    * is this a good idea though?
  * (complete) is called
  * (unsubscribe) is called
||#

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  nil)

;; TODO: handle setting complete? flag
(defstruct rx-observable
  "Reactive Extensions observable."
  (observable-fn #'noop)
  complete?)

;; TODO: overloads to take function(s) instead of observer
(defmethod rx-subscribe ((observable rx-observable) (observer rx-observer))
  (funcall (rx-observable-observable-fn observable) observer))

(defstruct rx-observer
  "Reactive Extensions observer."
  (next #'noop)
  (error #'noop)
  (complete #'noop))

(defparameter rx-print-observer
  (make-rx-observer :next (lambda (x) (format t "NEXT: ~A~%" x))
                    :error (lambda (x) (format t "ERROR: ~A~%" x))
                    :complete (lambda () (format t "COMPLETE~%")))
  "An Rx observer that simply prints everything it's given to standard out.")

(defun rx-range (start end)
  "Creates an observable range of numbers."
  (make-rx-observable
    :observable-fn
    (lambda (observer)
      (dotimes (i end)
        (funcall (rx-observer-next observer) i))
      (funcall (rx-observer-complete observer))
      #'noop)))

(defun rx-map (observable map-fn)
  "Reactive Extensions map operator."
  (make-rx-observable
    :observable-fn
    (lambda (observer)
      (rx-subscribe observable
                    (make-rx-observer
                      :next (lambda (x) (funcall (rx-observer-next observer) (funcall map-fn x)))
                      :error (lambda (x) (funcall (rx-observer-error observer) x))
                      :complete (lambda () (funcall (rx-observer-complete observer))))))))
