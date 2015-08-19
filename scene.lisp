#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric update (object))
(defgeneric clock (timer))
(defgeneric visibility (paintable))
(defgeneric paint (paintable target))
(defgeneric objects (container))
(defgeneric insert (container &rest objects))
(defgeneric withdraw (container &rest objects))
(defgeneric name-map (collective))
(defgeneric units (collective))
(defgeneric enter (unit collective))
(defgeneric leave (unit collective))
(defgeneric object (collective))
(defgeneric name (unit))
(defgeneric collective (unit))
(defgeneric scene (scene-unit))

(defmethod update (object)
  object)

(defmethod paint (paintable target)
  paintable)

(defclass timer ()
  ((clock :initarg :clock :accessor clock))
  (:default-initargs :clock 0))

(defclass paintable ()
  ((visibility :initarg :visibility :accessor visibility))
  (:default-initargs :visibility 1.0))

(defclass container ()
  ((objects :initform (make-indexed-set) :accessor objects)))

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
  (setf (collective unit) collective))

(defmethod leave ((unit unit) (container container))
  (withdraw container unit))

(defmethod leave ((unit unit) (collective collective))
  (call-next-method)
  (setf (collective unit) NIL))

(defclass scene (collective timer paintable)
  ())

(defclass scene-unit (unit)
  ())

(defmethod scene ((unit scene-unit))
  (collective unit))

(defmethod (setf scene) (scene (unit scene-unit))
  (setf (collective unit) scene))

(defclass location (vec scene-unit)
  ())

(defclass entity (container location paintable)
  ())

(defmethod enter ((entity entity) (container entity))
  (call-next-method)
  (when (collective container)
    (enter entity (collective container))))

(defmethod leave ((entity entity) (container entity))
  (call-next-method)
  (when (collective container)
    (leave entity (collective container))))

(defclass formation (entity)
  ())

(defclass particle (entity)
  ())
