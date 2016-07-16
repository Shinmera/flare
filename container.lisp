#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric name (unit))
(defgeneric enter (unit collective))
(defgeneric leave (unit collective))
(defgeneric clear (container))
(defgeneric objects (container))
(defgeneric name-map (collective))
(defgeneric units (collective))
(defgeneric unit (name collective))
(defgeneric collective (container-unit))

(define-self-returning-method clear (container))
(define-self-returning-method enter (unit collective))
(define-self-returning-method leave (unit collective))

(defclass unit ()
  ((name :initarg :name :reader name))
  (:default-initargs :name (gensym "")))

(defmethod initialize-instance :before ((unit unit) &key name)
  (check-type name (and symbol (not null))))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type T)
    (format stream "~a" (name unit))))

(defclass container ()
  ((objects :initarg :objects :accessor objects))
  (:default-initargs :objects (make-indexed-set)))

(defmethod paint ((container container))
  (for:for ((item over container))
    (paint item)))

(defmethod update ((container container))
  (for:for ((item over container))
    (update item)))

(defmethod enter (thing (container container))
  (set-add thing (objects container)))

(defmethod leave (thing (container container))
  (set-remove thing (objects container)))

(defmethod clear ((container container))
  (for:for ((item over container))
    (leave item container)))

(defmethod for:make-iterator ((container container) &rest args)
  (apply #'for:make-iterator (objects container) args))

(defun map-container-tree (function container)
  (for:for ((item over container))
    (funcall function item)
    (when (typep item 'container)
      (map-container-tree function item))))

(defmacro do-container-tree ((item container &optional return) &body body)
  `(progn (map-container-tree (lambda (,item) ,@body) ,container)
          ,return))

(defun print-container-tree (container &optional (depth 0))
  (format T "~&~v@{ ~}+ ~a~%" depth container)
  (for:for ((item over container))
    (if (typep item 'container)
        (visualize-container item (+ depth 2))
        (format T "~&~v@{ ~}| ~a~%" (+ depth 2) item))))

(defmethod describe-object ((container container) stream)
  (format stream "~a
  [~a]

Tree:"
          container (type-of container))
  (print-container-tree container stream)
  (format stream "~&"))

(defclass collective (container)
  ((name-map :initform (make-hash-table :test 'eq) :accessor name-map)))

(defmethod print-object ((collective collective) stream)
  (print-unreadable-object (collective stream :type T :identity T)
    (format stream "~a items" (hash-table-count (name-map collective)))))

(defmethod register ((unit unit) (collective collective))
  (setf (gethash (name unit) (name-map collective)) unit))

(defmethod deregister ((unit unit) (collective collective))
  (remhash (name unit) (name-map collective)))

(defmethod enter :after ((unit unit) (collective collective))
  (register unit collective))

(defmethod leave :after ((unit unit) (collective collective))
  (deregister unit collective))

(defmethod units ((collective collective))
  (let ((units ()))
    (do-container-tree (item collective units)
      (push item units))))

(defmethod unit ((name symbol) (collective collective))
  (gethash name (name-map collective)))

(defclass container-unit (container unit)
  ((collective :initform NIL :accessor collective)))

(defmethod print-object ((unit container-unit) stream)
  (print-unreadable-object (unit stream :type T)
    (format stream "~a => ~a" (name unit) (collective unit))))

(defmethod initialize-instance :after ((unit container-unit) &key collective)
  (setf (collective unit) collective))

(defmethod (setf collective) :before (collective (unit container-unit))
  (let ((collective (collective unit))))
  (when collective
    (do-container-tree (item collective)
      (deregister item collective))))

(defmethod (setf collective) :after ((collective collective) (unit container-unit))
  (when collective
    (do-container-tree (item collective)
      (register item collective))))

(defmethod enter :before ((unit container-unit) (collective collective))
  (when (collective unit)
    (error "~a is already contained in ~a, cannot enter it into ~a."
           unit (collective unit) collective)))

(defmethod leave :before ((unit container-unit) (collective collective))
  (unless (eql (collective unit) collective)
    (error "~a is contained in ~a, cannot leave it from ~a."
           unit (collective unit) collective)))

(defmethod enter :after ((unit unit) (container container-unit))
  (when (collective container)
    (register unit (collective container))))

(defmethod leave :after ((unit unit) (container container-unit))
  (when (collective container)
    (deregister unit (collective container))))

(defmethod register :after ((unit container-unit) (collective collective))
  (setf (collective unit) collective))

(defmethod deregister :after ((unit container-unit) (collective collective))
  (setf (collective unit) NIL))
