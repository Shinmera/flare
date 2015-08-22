#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric call-with-translation (func target vec))
(defgeneric update (object))
(defgeneric stop (timer))
(defgeneric running (timer))
(defgeneric start (timer))
(defgeneric reset (timer))
(defgeneric clock (timer))
(defgeneric visibility (paintable))
(defgeneric paint (paintable target))
(defgeneric objects (container))
(defgeneric insert (container &rest objects))
(defgeneric withdraw (container &rest objects))
(defgeneric name-map (collective))
(defgeneric units (collective))
(defgeneric unit (name collective))
(defgeneric enter (unit collective))
(defgeneric leave (unit collective))
(defgeneric object (collective))
(defgeneric name (unit))
(defgeneric collective (unit))
(defgeneric progressions (scene))
(defgeneric add-progression (progression scene))
(defgeneric remove-progression (progression scene))
(defgeneric scene (scene-unit))
(defgeneric location (entity))
(defgeneric orientation (entity))
(defgeneric size (entity))
(defgeneric up (entity))
(defgeneric angle (entity))

(defmethod update (object)
  object)

(defmethod paint (paintable target)
  paintable)

(defclass target ()
  ())

(defmacro with-translation ((vec target) &body body)
  `(call-with-translation (lambda () ,@body) ,target ,vec))

(defclass timer ()
  ((previous-time :initform NIL :accessor previous-time)
   (clock :initarg :clock :accessor clock))
  (:default-initargs :clock 0.0s0))

(defmethod print-object ((timer timer) stream)
  (print-unreadable-object (timer stream :type T :identity T)
    (format stream "~s ~a" (if (previous-time timer) :running :stopped) (clock timer))))

(defmethod running ((timer timer))
  (not (null (previous-time timer))))

(defmethod reset ((timer timer))
  (setf (clock timer) 0.0s0)
  (when (previous-time timer)
    (setf (previous-time timer) (get-internal-real-time)))
  timer)

(defmethod stop ((timer timer))
  (setf (previous-time timer) NIL)
  timer)

(defmethod start ((timer timer))
  (setf (previous-time timer) (get-internal-real-time))
  timer)

(defmethod update :before ((timer timer))
  (let ((new-time (get-internal-real-time)))
    (incf (clock timer)
          (float (/ (- new-time (previous-time timer))
                    internal-time-units-per-second)
                 1.0s0))
    (setf (previous-time timer) new-time)))

(defmethod update :around ((timer timer))
  (when (running timer)
    (call-next-method)))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

(defclass container ()
  ((objects :initform (make-indexed-set) :accessor objects)))

(defun map-container-tree (function container)
  (labels ((traverse (container)
             (when (typep container 'container)
               (do-set (i container) (objects container)
                 (declare (ignore i))
                 (funcall function container)
                 (traverse container)))))
    (traverse container)))

(defmacro do-container-tree ((object container) &body body)
  `(block NIL (map-container-tree (lambda (,object) ,@body) ,container)))

(defun print-container-tree (container)
  (labels ((print-container (container level)
             (format T "~&~a ~a~%" (make-string (* level 2) :initial-element #\ ) container)
             (when (typep container 'container)
               (do-set (i container) (objects container)
                 (declare (ignore i))
                 (print-container container (1+ level))))))
    (print-container container 0)))

(defmethod insert ((container container) &rest objects)
  (dolist (obj objects)
    (set-add obj (objects container)))
  container)

(defmethod withdraw ((container container) &rest objects)
  (dolist (obj objects)
    (set-remove obj (objects container)))
  container)

(defmethod paint ((container container) target)
  (do-set (i obj) (objects container)
    (declare (ignore i))
    (paint obj target)))

(defclass collective (container)
  ((name-map :initform (make-hash-table :test 'eql) :accessor name-map)))

(defmethod units ((collective collective))
  (objects collective))

(defmethod unit (name (collective collective))
  (gethash name (name-map collective)))

(defclass unit ()
  ((name :initarg :name :accessor name)
   (collective :initarg :collective :accessor collective))
  (:default-initargs
   :name NIL
   :collective NIL))

(defmethod (setf name) :before (name (unit unit))
  (when (collective unit)
    (when (name unit)
      (remhash (name unit) (name-map (collective unit))))
    (when name
      (setf (gethash name (name-map (collective unit))) unit))))

(defmethod enter ((unit unit) (container container))
  (insert container unit))

(defmethod enter ((unit unit) (collective collective))
  (call-next-method)
  (setf (collective unit) collective)
  (setf (gethash (name unit) (name-map collective)) unit))

(defmethod leave ((unit unit) (container container))
  (withdraw container unit))

(defmethod leave ((unit unit) (collective collective))
  (call-next-method)
  (setf (collective unit) NIL)
  (remhash (name unit) (name-map collective)))

(defmethod leave ((unit unit) (collective (eql T)))
  (leave unit (collective unit)))

(defmethod unit (name (unit unit))
  (unit name (collective unit)))

(defclass scene (collective timer paintable)
  ((progressions :initform (make-hash-table :test 'eql) :accessor progressions)))

(defmethod update ((scene scene))
  (maphash (lambda (name progression)
             (declare (ignore name))
             (tick progression scene))
           (progressions scene)))

(defmethod progression (name (scene scene))
  (gethash name (progressions scene)))

(defmethod add-progression (progression (scene scene))
  (setf (gethash (name progression) (progressions scene)) progression))

(defmethod remove-progression (progression (scene scene))
  (remhash (name progression) (progressions scene)))

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
  (setf (tangent ring) (cross (up ring) (orientation ring))))

(defmethod initialize-instance :after ((ring ring) &key) (%prepare-ring ring))
(defmethod (setf up) :after (val (ring ring)) (%prepare-ring ring))
(defmethod (setf orientation) :after (val (ring ring)) (%prepare-ring ring))

(defmethod paint ((ring ring) target)
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
      ;; Paint at proper location
      do (with-translation (p target)
           (paint (flare-queue::value current) target)))))
