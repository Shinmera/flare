#|
This file is a part of flare
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.vector)

(defgeneric copy (object))

(defgeneric x (vec))
(defgeneric y (vec))
(defgeneric z (vec))
(defgeneric size (vec))
(defgeneric translate (vec other))
(defgeneric scale (vec other))
(defgeneric rotate-around (vec axis phi))
(defgeneric rotate (vec other))
(defgeneric normalize (vec))
(defgeneric translated (vec other))
(defgeneric scaled (vec other))
(defgeneric rotated (vec other))
(defgeneric normalized (vec))
(defgeneric cross (a b))
(defgeneric dot (a b))

(defclass vec ()
  ((x :initarg :x :accessor x :type double-float)
   (y :initarg :y :accessor y :type double-float)
   (z :initarg :z :accessor z :type double-float))
  (:default-initargs
   :x 0 :y 0 :z 0))

(defmethod print-object ((v vec) stream)
  (if (eql (type-of v) 'vec)
      (print (make-load-form v) stream)
      (call-next-method)))

(defmethod make-load-form ((v vec) &optional env)
  (declare (ignore env))
  `(vec ,(x v) ,(y v) ,(z v)))

(defun vec (x y z)
  (make-instance 'vec :x x :y y :z z))

(defmethod copy ((v vec))
  (make-instance 'vec :x (x v) :y (y v) :z (z v)))

(defmethod size ((v vec))
  (sqrt (+ (expt (x v) 2)
           (expt (y v) 2)
           (expt (z v) 2))))

(defmacro vmodf (func place &rest args)
  `(setf ,place (,func ,place ,@args)))

(defmethod translate ((v vec) (other vec))
  (vmodf + (x v) (x other))
  (vmodf + (y v) (y other))
  (vmodf + (z v) (z other))
  v)

(defmethod translate ((v vec) (other real))
  (vmodf + (x v) other)
  (vmodf + (y v) other)
  (vmodf + (z v) other)
  v)

(defmethod scale ((v vec) (other vec))
  (vmodf * (x v) (x other))
  (vmodf * (y v) (y other))
  (vmodf * (z v) (z other))
  v)

(defmethod scale ((v vec) (other real))
  (vmodf * (x v) other)
  (vmodf * (y v) other)
  (vmodf * (z v) other)
  v)

(defmethod rotate-around ((v vec) (k vec) (phi real))
  ;; https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  ;; vr = v*cos(phi) + (kxv)*sin(phi) + k*(k*v)*(1-cos(phi)
  (let ((cos (cos phi))
        (sin (sin phi))
        (c (cross k v))
        (d (dot k v)))
    (macrolet ((arith (field)
                 `(+ (* (,field v) cos)
                     (* (,field c) sin)
                     (* (,field k) d (- 1 cos)))))
      (psetf (x v) (arith x)
             (y v) (arith y)
             (z v) (arith z)))
    v))

(defmethod rotate ((v vec) (other vec))
  (unless (= 0 (x other))
    (rotate-around v (vec 1 0 0) (x other)))
  (unless (= 0 (y other))
    (rotate-around v (vec 0 1 0) (y other)))
  (unless (= 0 (z other))
    (rotate-around v (vec 0 0 1) (z other)))
  v)

(defmethod normalize ((v vec))
  (let ((size (size v)))
    (vmodf / (x v) size)
    (vmodf / (y v) size)
    (vmodf / (z v) size))
  v)

(defmethod translated ((v vec) other)
  (translate (copy v) other))

(defmethod scaled ((v vec) other)
  (scale (copy v) other))

(defmethod rotated-around ((v vec) (k vec) (phi real))
  (rotate-around (copy v) k phi))

(defmethod rotated ((v vec) other)
  (rotate (copy v) other))

(defmethod normalized ((v vec))
  (normalize (copy v)))

(defmethod cross ((a vec) (b vec))
  (vec (- (* (y a) (z b))
          (* (z a) (y b)))
       (- (* (z a) (x b))
          (* (x a) (z b)))
       (- (* (x a) (y b))
          (* (y a) (x b)))))

(defmethod dot ((a vec) (b vec))
  (+ (* (x a) (x b))
     (* (y a) (y b))
     (* (z a) (z b))))
