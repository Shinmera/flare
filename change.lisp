#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric parse-change (type args))

(defmacro define-change-parser (name args &body body)
  (let ((form (gensym "FORM")))
    `(defmethod parse-change ((,(gensym) (eql ',name)) ,form)
       (destructuring-bind ,args ,form
         ,@body))))

(defclass change ()
  ())

(defclass print-change (change)
  ())

(defmethod tick ((change print-change) object clock step)
  (format T "~a ~a ~a" object clock step))

(defmethod copy ((change print-change))
  (make-instance 'print-change))

(defmethod reset ((change print-change)))

(define-change-parser print ()
  `(make-instance 'print-change))

(defclass call-change (change)
  ((func :initarg :func :accessor func)))

(defmethod tick ((change call-change) object clock step)
  (funcall (func change) object clock step))

(defmethod copy ((change call-change))
  (make-instance 'call-change :func (func change)))

(defmethod reset ((change call-change)))

(define-change-parser call (function)
  `(make-instance 'call-change :func ,function))

(defclass operation (change)
  ())

(defclass enter-operation (operation)
  ((objects :initform (make-hash-table :test 'eq) :accessor objects)
   (creator :initarg :creator :accessor creator)))

(defmethod reset ((op enter-operation))
  (loop for object being the hash-keys of (objects op)
        for collective being the hash-values of (objects op)
        do (leave object collective)
           (remhash object (objects op))))

(defmethod tick ((op enter-operation) target clock step)
  (flet ((register (object)
           (unless (collective object)
             (setf (gethash object (objects op)) target)
             (enter object target))))
    (let ((obj (funcall (creator op))))
      (if (listp obj)
          (mapc #'register obj)
          (register obj)))))

(defmethod copy ((op enter-operation))
  (make-instance 'enter-operation :creator (creator op)))

(define-change-parser create (class &rest initargs &key n children parent &allow-other-keys)
  (remf initargs :n)
  (remf initargs :children)
  (remf initargs :parent)
  (let* ((instance (gensym "INSTANCE"))
         (inner `(let ((,instance (make-instance ',class ,@initargs)))
                   ,@(when children (list (parse-change 'create `(,@children :parent ,instance))))
                   ,@(when parent (list `(enter ,instance ,parent)))
                   ,instance)))
    (if n
        `(loop repeat ,n
               collect ,inner)
        inner)))

(define-change-parser enter (&rest args)
  `(make-instance
    'enter-operation
    :creator (lambda () ,(parse-change 'create args))))

(defclass leave-operation (operation)
  ((objects :initform (make-hash-table :test 'eq) :accessor objects)))

(defmethod reset ((op leave-operation))
  (loop for object being the hash-keys of (objects op)
        for collective being the hash-values of (objects op)
        do (enter object collective)
           (remhash object (objects op))))

(defmethod tick ((op leave-operation) object clock step)
  (when (collective object)
    (setf (gethash object (objects op)) (collective object))
    (leave object T)))

(defmethod copy ((op leave-operation))
  (make-instance 'leave-operation))

(define-change-parser leave ()
  `(make-instance 'leave-operation))

(defclass tween (change)
  ())

(defclass slot-tween (tween)
  ((slot :initarg :slot :accessor slot)
   (originals :initform (make-hash-table :test 'eq) :accessor originals)))

(defmethod original-value (object (tween slot-tween))
  (or (gethash object (originals tween))
      (setf (gethash object (originals tween))
            (slot-value object (slot tween)))))

(defmethod reset ((tween slot-tween))
  (loop for object being the hash-keys of (originals tween)
        for value being the hash-values of (originals tween)
        do (setf (slot-value object (slot tween)) value)
           (remhash object (originals tween))))

(defclass range-tween (tween)
  ((from :initarg :from :accessor from)
   (to :initarg :to :accessor to)
   (ease-func :initarg :ease :accessor ease-func))
  (:default-initargs
   :from 0
   :to 1
   :ease 'linear))

(defclass range-slot-tween (range-tween slot-tween)
  ())

(defmethod tick ((tween range-slot-tween) object clock step)
  (setf (slot-value object (slot tween))
        (etypecase (from tween)
          (number (ease step (ease-func tween) (from tween) (to tween)))
          (vec (ease-vec step (ease-func tween) (from tween) (to tween))))))

(defmethod copy ((tween range-slot-tween))
  (make-instance 'range-slot-tween
                 :slot (slot tween)
                 :from (from tween)
                 :to (to tween)
                 :ease (ease-func tween)))

(define-change-parser set (slot &key (from 0) (to 1) (ease 'linear))
  `(make-instance 'range-slot-tween :ease ',ease :from ,from :to ,to :slot ',slot))

(defclass constant-tween (tween)
  ((by :initarg :by :accessor by)
   (for :initarg :for :accessor for))
  (:default-initargs
   :by 1
   :for 1))

(defclass increase-slot-tween (constant-tween slot-tween)
  ())

(defmethod tick ((tween increase-slot-tween) object clock step)
  (setf (slot-value object (slot tween))
        (etypecase (by tween)
          (number (+ (original-value object tween) (* (by tween) (/ clock (for tween)))))
          (vec (v+ (original-value object tween) (v* (by tween) (/ clock (for tween))))))))

(defmethod copy ((tween increase-slot-tween))
  (make-instance 'increase-slot-tween
                 :slot (slot tween)
                 :by (by tween)
                 :for (for tween)))

(define-change-parser increase (slot &key (by 1) (for 1))
  `(make-instance 'increase-slot-tween :by ,by :for ,for :slot ',slot))
