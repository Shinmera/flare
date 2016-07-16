#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric location (entity))

(defclass scene (collective clock paintable animatable)
  ())

(defmethod start :after ((scene scene))
  (dolist (progression (progressions scene))
    (setf (previous-time progression) (previous-time scene))))

(defclass entity (container-unit paintable animatable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))
