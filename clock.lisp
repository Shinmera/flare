#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric update (object))
(defgeneric stop (clock))
(defgeneric start (clock))
(defgeneric reset (clock))
(defgeneric running (clock))
(defgeneric synchronize (clock new))
(defgeneric clock (clock))

(defmethod update (object)
  object)

(define-self-returning-method update (object))

(defclass clock ()
  ((previous-time :initform (get-internal-real-time) :accessor previous-time)
   (clock :initarg :clock :accessor clock)
   (running :initarg :running :accessor running))
  (:default-initargs
   :clock 0.0s0
   :running NIL))

(define-self-returning-method stop ((clock clock)))
(define-self-returning-method start ((clock clock)))
(define-self-returning-method reset (clock))
(define-self-returning-method synchronize (clock new))

(defmethod describe-object ((clock clock) stream)
  (format stream "~&~a
  [~a]

The clock is ~:[STOPPED~;RUNNING~]
Internal clock is at ~a~&"
          clock (type-of clock) (running clock) (clock clock)))

(defmethod print-object ((clock clock) stream)
  (print-unreadable-object (clock stream :type T :identity T)
    (format stream "~s ~a" (if (running clock) :running :stopped) (clock clock))))

(defmethod reset ((clock clock))
  (setf (clock clock) 0.0s0)
  (setf (previous-time clock) (get-internal-real-time)))

(defmethod synchronize ((clock clock) (with clock))
  (setf (clock clock) (clock with)
        (previous-time clock) (previous-time with)))

(defmethod synchronize ((clock clock) (with real))
  (setf (clock clock) (float with 0.0s0)
        (previous-time clock) (get-internal-real-time)))

(defmethod stop ((clock clock))
  (setf (running clock) NIL))

(defmethod start ((clock clock))
  (setf (previous-time clock) (get-internal-real-time))
  (setf (running clock) T))

(defmethod update :before ((clock clock))
  (let ((new-time (get-internal-real-time)))
    (incf (clock clock)
          (float (/ (- new-time (previous-time clock))
                    internal-time-units-per-second)
                 1.0s0))
    (setf (previous-time clock) new-time)))

(defmethod update :around ((clock clock))
  (when (running clock)
    (call-next-method)))
