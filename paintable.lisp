(in-package #:org.shirakumo.flare)

(defgeneric call-with-translation (func target vec))
(defgeneric visibility (paintable))
(defgeneric paint (paintable target))

(defmethod paint (paintable target)
  paintable)

(defmacro with-translation ((vec target) &body body)
  `(call-with-translation (lambda () ,@body) ,target ,vec))

(defclass target ()
  ())

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))
