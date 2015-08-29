#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric progressions (scene))
(defgeneric progression (name scene))
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

(defclass entity (collective scene-unit paintable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))

(defmethod paint ((entity entity) target)
  (do-set (obj (objects entity))
    (paint obj target)))

(defmethod enter ((entity entity) (container entity))
  (call-next-method)
  ;; (when (and (name entity) (collective container))
  ;;   (setf (gethash (name entity) (name-map (collective container))) entity))
  )

(defmethod leave ((entity entity) (container entity))
  (call-next-method)
  ;; (when (and (name entity) (collective container))
  ;;   (remhash (name entity) (name-map (collective container))))
  )

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
  (setf (tangent ring) (vc (up ring) (orientation ring)))
  (let ((step (/ 360 (max 1 (set-size (objects ring)))))
        (offset (angle ring)))
    (iterate (for child in-set (objects ring))
      (for deg from offset below (+ offset 360) by step)
      (for phi = (* deg Pi 1/180))
      (for u = (* (size ring) (cos phi)))
      (for v = (* (size ring) (sin phi)))
      (for p = (vec (+ (vx (location ring))
                       (* u (vx (orientation ring)))
                       (* v (vx (tangent ring))))
                    (+ (vy (location ring))
                       (* u (vy (orientation ring)))
                       (* v (vy (tangent ring))))
                    (+ (vz (location ring))
                       (* u (vz (orientation ring)))
                       (* v (vz (tangent ring))))))
      (setf (location child) p))))

(defmethod initialize-instance :after ((ring ring) &key) (%prepare-ring ring))
(defmethod (setf up) :after (val (ring ring)) (%prepare-ring ring))
(defmethod (setf orientation) :after (val (ring ring)) (%prepare-ring ring))
(defmethod (setf angle) :after (val (ring ring)) (%prepare-ring ring))
(defmethod insert :after ((ring ring) &rest objs) (declare (ignore objs)) (%prepare-ring ring))
(defmethod withdraw :after ((ring ring) &rest objs) (declare (ignore objs)) (%prepare-ring ring))

(defmethod update :before ((ring ring))
  (%prepare-ring ring))

(defmethod paint ((ring ring) target)
  (do-set (current (objects ring))
    (paint current target)))
