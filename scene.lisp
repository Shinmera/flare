#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric scene (scene-unit))
(defgeneric location (entity))
(defgeneric orientation (entity))
(defgeneric size (entity))
(defgeneric up (entity))
(defgeneric angle (entity))

(defclass scene (collective clock paintable animatable)
  ())

(defmethod start :after ((scene scene))
  (loop for progression being the hash-values of (progressions scene)
        do (setf (previous-time progression) (previous-time scene))))

(defmethod update ((scene scene))
  (call-next-method))

(defmethod paint ((scene scene) target)
  (do-set (obj (objects scene))
    (paint obj target)))

(defmethod progression (name (scene scene))
  (gethash name (progressions scene)))

(defmethod enter ((name symbol) (scene scene))
  (enter (make-instance 'progression :name name) scene))

(defmethod enter ((progression progression) (scene scene))
  (let ((found (gethash (name progression) (progressions scene))))
    (when (and found (not (eql progression found)))
      (cerror "Replace the progression."
              "A different progression with the same name ~s already exists on ~a."
              (name progression) scene)))
  (setf (gethash (name progression) (progressions scene)) progression))

(defmethod leave ((name symbol) (scene scene))
  (remhash name (progressions scene)))

(defmethod leave ((progression progression) (scene scene))
  (leave (name progression) scene))

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
