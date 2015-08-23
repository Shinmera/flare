#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

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

(defclass container ()
  ((objects :initform (make-indexed-set) :accessor objects)))

(defmethod describe-object ((container container) stream)
  (format stream "~a
  [~a]

Tree:"
          container (type-of container))
  (print-container-tree container stream))

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

(defun print-container-tree (container &optional (stream T))
  (labels ((print-container (container level)
             (format stream "~&~a ~a~%" (make-string (* level 2) :initial-element #\ ) container)
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