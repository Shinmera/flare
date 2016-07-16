#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric orientation (entity))
(defgeneric size (entity))
(defgeneric up (arc))
(defgeneric angle (arc))
(defgeneric spacing (arc))

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

(defgeneric reposition (formation))

(defmethod initialize-instance :after ((formation formation) &key)
  (reposition formation))

(defmethod insert :after ((formation formation) &rest objs)
  (declare (ignore objs))
  (reposition formation))

(defmethod withdraw :after ((formation formation) &rest objs)
  (declare (ignore objs))
  (reposition formation))

(defmethod (setf location) :after (value (formation formation))
  (reposition formation))

(defclass particle (entity)
  ())

(defclass arc (formation oriented-entity sized-entity)
  ((up :initarg :up :accessor up)
   (tangent :accessor tangent)
   (angle :initarg :angle :accessor angle)
   (spacing :initarg :spacing :accessor spacing))
  (:default-initargs
   :up (vec 0 0 1)
   :angle 0
   :spacing 10
   :size 0))

(defmethod print-object ((arc arc) stream)
  (print-unreadable-object (arc stream :type T :identity T)
    (format stream "~s ~s ~s ~s" :spacing (spacing arc) :angle (angle arc))))

(defmethod reposition ((arc arc))
  (with-slots (up tangent angle spacing size orientation location) arc
    (setf tangent (vc up orientation))
    (let ((offset (- angle (/ (* (set-size (objects arc)) spacing) 2))))
      (for:for ((child over (objects arc))
                (deg from angle :by spacing)
                (phi = (* deg Pi 1/180))
                (u = (* size (cos phi)))
                (v = (* size (sin phi)))
                (p = (vec (+ (vx location)
                             (* u (vx orientation))
                             (* v (vx tangent)))
                          (+ (vy location)
                             (* u (vy orientation))
                             (* v (vy tangent)))
                          (+ (vz location)
                             (* u (vz orientation))
                             (* v (vz tangent))))))
        (setf (location child) p)))))

(defmethod (setf up) :after (val (arc arc))
  (reposition arc))

(defmethod (setf orientation) :after (val (arc arc))
  (reposition arc))

(defmethod (setf angle) :after (val (arc arc))
  (reposition arc))

(defmethod (setf size) :after (val (arc arc))
  (reposition arc))

(defmethod (setf spacing) :after (val (arc arc))
  (reposition arc))

(defclass ring (arc)
  ())

;; Can't be :AFTER as they would happen after the REPOSITION
;; call that ARC performs, which is not what we need.
(defmethod reposition :before ((ring ring))
  (let ((size (set-size (objects ring))))
    (setf (slot-value ring 'spacing)
          (if (<= size 0) 0 (/ 360 size)))))
