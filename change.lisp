#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric compile-change (name args))
(defgeneric field (tween))
(defgeneric from (tween))
(defgeneric to (tween))
(defgeneric from (tween))
(defgeneric by (tween))
(defgeneric ease-func (tween))

(defclass change ()
  ())

(defclass tween (change)
  ((field :initarg :field :accessor field)
   (from :initarg :from :accessor from)
   (to :initarg :to :accessor to)
   (by :initarg :by :accessor by)
   (ease :initarg :ease :accessor ease-func)
   (initials :initform (make-hash-table :test 'eq) :accessor initials))
  (:default-initargs
   :from NIL :to NIL :by NIL :ease 'linear))

(defmethod initialize-instance :after ((tween tween) &key)
  (when (and (not (to tween))
             (not (by tween)))
    (error "Must specify either TO or BY.")))

(defmethod initial-value ((tween tween) object)
  (or (gethash object (initials tween))
      (setf (gethash object (initials tween))
            (slot-value object (field tween)))))

(defclass scale (tween)
  ())

(defmethod perform ((tween scale) object clock step)
  (setf (slot-value object (field tween))
        (scaled (initial-value tween object)
                (ease-vec step (ease-func tween) (from tween) (to tween)))))

(defclass rotate (tween)
  ())

(defmethod perform ((tween rotate) object clock step)
  (setf (slot-value object (field tween))
        (rotated (initial-value tween object)
                 (ease-vec step (ease-func tween) (from tween) (to tween)))))

(defclass set! (tween)
  ())

(defmethod perform ((tween set!) object clock step)
  (setf (slot-value object (field tween))
        (ease step (ease-func tween) (or (from tween) (initial-value tween object)) (to tween))))

(defclass increase (tween)
  ())

(defmethod perform ((tween increase) object clock step)
  (incf (slot-value object (field tween))
        (by tween)))

(defclass edit (change)
  ((done :initform NIL :accessor done)))

(defmethod perform :around ((edit edit) object clock step)
  (unless (done edit)
    (call-next-method))
  (setf (done edit) T))

(defclass enter (change)
  ((object :initarg :object :accessor object)))

(defmethod perform ((enter enter) object clock step)
  (enter (object enter) object))

(defclass leave (change)
  ((object :initarg :object :accessor object))
  (:default-initargs
   :object NIL))

(defmethod perform ((change leave) object clock step)
  (if (object change)
      (leave object (object change))
      (leave object T)))

(defclass delegating-change (change)
  ((change :initarg :change :accessor change)))

(defclass every. (change)
  ((distance :initarg :distance :accessor distance)
   (previous-time :initform NIL :accessor previous-time)))

(defmethod perform ((change every.) object clock step)
  (cond ((not (previous-time change))
         (setf (previous-time change) clock))
        ((< (distance change)
            (- clock (previous-time change)))
         (perform (change change) object clock step)
         (setf (previous-time change) clock))))
