#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

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
