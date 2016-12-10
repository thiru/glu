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

(defun rx-map (observable map-fn)
  "Reactive Extensions map operator."
  (lambda () 23)
  )

(defun rx-map (map-fn next &key on-error on-complete)
  "Reactive Extensions map operator."
  43
  ; return teardown func
 )

(defun rx-subscribe (next &key error complete)
  "Reactive Extensions subscribe."
  )
||#

(defstruct observer
  "Reactive Extensions observer."
  closed?
  )

(defgeneric rx-next (observer value)
  (:documentation "Get the next item from the observable (TODO)."))

(defgeneric rx-error (observer err)
  (:documentation "Report error (TODO)"))

(defgeneric rx-complete (observer)
  (:documentation "Report complete (TODO)")
  )

(defmethod rx-next ((obs observer) (val t))
  (format t "NEXT: ~A~%" val))

(defmethod rx-complete ((obs observer))
  (setf (observer-closed? obs) t)
  (format t "Complete~%"))

(defmethod rx-error ((obs observer) (err t))
  (setf (observer-closed? obs) t)
  (format t "Error: ~A~%" err)
  )

(defun rx-range (observer start end)
  "Creates an observable range of numbers."
  (dotimes (i end)
    (rx-next observer i)
    )
  (rx-complete observer)
  (lambda () t)
  )

