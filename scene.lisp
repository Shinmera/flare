#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric progressions (scene))
(defgeneric scene (scene-unit))
(defgeneric location (entity))
(defgeneric orientation (entity))
(defgeneric size (entity))
(defgeneric up (entity))
(defgeneric angle (entity))

(defclass scene (collective clock paintable)
  ((progressions :initform (make-hash-table :test 'eql) :accessor progressions)))

(defmethod describe-object ((scene scene) stream)
  (format stream "~a
  [~a]

Internal clock is at ~a
Progressions:
  ~{~a~^~%  ~}
Tree:"
          scene (type-of scene) (clock scene)
          (loop for v being the hash-values of (progressions scene) collect v))
  (print-container-tree scene stream)
  (format stream "~&"))

(defmethod start :after ((scene scene))
  (loop for progression being the hash-values of (progressions scene)
        do (setf (previous-time progression) (previous-time scene))))

(defmethod update ((scene scene))
  (call-next-method)
  (loop for progression being the hash-values of (progressions scene)
        do (tick progression T scene)))

(defmethod paint ((scene scene) target)
  (do-set (i obj) (objects scene)
    (declare (ignore i))
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

(defclass entity (container scene-unit paintable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod paint ((entity entity) target)
  (do-set (i obj) (objects entity)
    (declare (ignore i))
    (paint obj target)))

(defmethod enter ((entity entity) (container entity))
  (call-next-method)
  (when (and (name entity) (collective container))
    (setf (gethash (name entity) (name-map (collective container))) entity)))

(defmethod leave ((entity entity) (container entity))
  (call-next-method)
  (when (and (name entity) (collective container))
    (remhash (name entity) (name-map (collective container)))))

(defclass oriented-entity (entity)
  ((orientation :initarg :orientation :accessor orientation))
  (:default-initargs
   :orientation (vec 1 0 0)))

(defclass sized-entity (entity)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size 1.0))

(defclass formation (entity)
  ())

(defclass particle (entity)
  ())

(defclass ring (formation oriented-entity sized-entity)
  ((up :initarg :up :accessor up)
   (tangent :accessor tangent)
   (angle :initarg :angle :accessor angle))
  (:default-initargs
   :up (vec 0 0 1)
   :angle 0))

(defun %prepare-ring (ring)
  (setf (tangent ring) (cross (up ring) (orientation ring)))
  (let ((step (/ 360 (max 1 (set-size (objects ring)))))
        (offset (angle ring)))
    (loop-set (current) (objects ring)
      ;; Step the angle
      for deg from offset below (+ offset 360) by step
      for phi = (* deg Pi 1/180)
      ;; Translate to cartesian
      for u = (* (size ring) (cos phi))
      for v = (* (size ring) (sin phi))
      ;; Translate into ring plane
      for p = (vec (+ (x (location ring))
                      (* u (x (orientation ring)))
                      (* v (x (tangent ring))))
                   (+ (y (location ring))
                      (* u (y (orientation ring)))
                      (* v (y (tangent ring))))
                   (+ (z (location ring))
                      (* u (z (orientation ring)))
                      (* v (z (tangent ring)))))
      ;; Set to proper location
      do (setf (location (flare-queue::value current)) p))))

(defmethod initialize-instance :after ((ring ring) &key) (%prepare-ring ring))
(defmethod (setf up) :after (val (ring ring)) (%prepare-ring ring))
(defmethod (setf orientation) :after (val (ring ring)) (%prepare-ring ring))
(defmethod (setf angle) :after (val (ring ring)) (%prepare-ring ring))
(defmethod insert :after ((ring ring) &rest objs) (%prepare-ring ring))
(defmethod withdraw :after ((ring ring) &rest objs) (%prepare-ring ring))

(defmethod update :before ((ring ring))
  (%prepare-ring ring))

(defmethod paint ((ring ring) target)
  (do-set (i current) (objects ring)
    (declare (ignore i))
    (paint current target)))
