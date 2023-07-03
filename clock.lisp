(in-package #:org.shirakumo.flare)

(defgeneric update (object dt))
(defgeneric stop (clock))
(defgeneric start (clock))
(defgeneric reset (clock))
(defgeneric running (clock))
(defgeneric timescale (clock))
(defgeneric synchronize (clock new))
(defgeneric clock (clock))

(defmethod update (object dt)
  object)

(define-self-returning-method update (object dt))

(defclass clock ()
  ((clock :initarg :clock :accessor clock)
   (running :initarg :running :accessor running))
  (:default-initargs
   :clock 0.0f0
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
  (setf (clock clock) 0.0f0))

(defmethod synchronize ((clock clock) (with clock))
  (setf (clock clock) (clock with)))

(defmethod synchronize ((clock clock) (with real))
  (setf (clock clock) (float with 0.0f0)))

(defmethod stop ((clock clock))
  (setf (running clock) NIL))

(defmethod start ((clock clock))
  (setf (running clock) T))

(defmethod update :before ((clock clock) dt)
  (incf (clock clock) dt))

(defmethod update :around ((clock clock) dt)
  (when (running clock)
    (call-next-method)))
