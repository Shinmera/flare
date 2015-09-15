#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric scene (scene-unit))
(defgeneric location (entity))

(defclass scene (collective clock paintable animatable)
  ())

(defmethod start :after ((scene scene))
  (dolist (progression (progressions scene))
    (setf (previous-time progression) (previous-time scene))))

(defmethod update ((scene scene))
  (call-next-method))

(defmethod paint ((scene scene) target)
  (do-set (obj (objects scene))
    (paint obj target)))

(defclass scene-unit (unit)
  ())

(defmethod scene ((unit scene-unit))
  (collective unit))

(defmethod (setf scene) (scene (unit scene-unit))
  (setf (collective unit) scene))

(defclass entity (collective scene-unit paintable animatable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod paint ((entity entity) target)
  (do-set (obj (objects entity))
    (paint obj target)))

(defmethod enter ((entity entity) (container entity))
  (call-next-method))

(defmethod leave ((entity entity) (container entity))
  (call-next-method))
