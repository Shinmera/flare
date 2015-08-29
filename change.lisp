#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric perform (change object clock step))
(defgeneric done (change))
(defgeneric field (tween))
(defgeneric from (tween))
(defgeneric to (tween))
(defgeneric from (tween))
(defgeneric by (tween))
(defgeneric ease-func (tween))
(defgeneric initial-value (tween object))

(defclass change ()
  ((done :initform NIL :accessor done)))

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

(defmethod print-object ((tween tween) stream)
  (print-unreadable-object (tween stream :type T)
    (format stream "~s ~s ~s ~s ~s"
            :from (from tween) :to (to tween) (ease-func tween))))

(defmethod initial-value ((tween tween) object)
  (or (gethash object (initials tween))
      (setf (gethash object (initials tween))
            (slot-value object (field tween)))))

(defmethod reset ((tween tween))
  (setf (initials tween) (make-hash-table :test 'eq)))

(defclass scale (tween)
  ())

(defmethod perform ((tween scale) object clock step)
  (setf (slot-value object (field tween))
        (v* (initial-value tween object)
            (ease-vec step (ease-func tween) (from tween) (to tween)))))

(defclass rotate (tween)
  ())

(defmethod perform ((tween rotate) object clock step)
  (setf (slot-value object (field tween))
        (vrotv (initial-value tween object)
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
  ())

(defmethod reset ((edit edit))
  (setf (done edit) NIL))

(defmethod perform :after ((edit edit) object clock step)
  (setf (done edit) T))

(defclass enter (edit)
  ((object :initarg :object :accessor object)))

(defmethod perform ((enter enter) object clock step)
  (enter (funcall (object enter)) object))

(defclass leave (edit)
  ((object :initarg :object :accessor object))
  (:default-initargs
   :object T))

(defmethod perform ((change leave) object clock step)
  (leave object (object change)))

(defclass delegating-change (change)
  ((change :initarg :change :accessor change)))

(defclass every. (delegating-change)
  ((distance :initarg :distance :accessor distance)
   (previous-time :initform NIL :accessor previous-time)))

(defmethod perform ((change every.) object clock step)
  (cond ((not (previous-time change))
         (setf (previous-time change) clock))
        ((< (distance change)
            (- clock (previous-time change)))
         (perform (change change) object clock step)
         (setf (previous-time change) clock))))
