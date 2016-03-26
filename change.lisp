#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric parse-change (type args))

(defmacro define-change-parser (type args &body body)
  (let ((form (gensym "FORM")))
    `(defmethod parse-change ((,(gensym) (eql ',type)) ,form)
       (destructuring-bind ,args ,form
         ,@body))))

(defclass change ()
  ())

(defmethod copy ((change change))
  (make-instance (class-name (class-of change))))

(defmethod reset ((change change))
  change)

(defclass print-change (change)
  ())

(defmethod tick ((change print-change) object clock step)
  (format T "~a ~a ~a" object clock step))

(define-change-parser print ()
  `(make-instance 'print-change))

(defclass call-change (change)
  ((func :initarg :func :accessor func)))

(defmethod tick ((change call-change) object clock step)
  (funcall (func change) object clock step))

(defmethod copy :around ((change call-change))
  (let ((c (call-next-method)))
    (setf (func c) (func change))
    c))

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
  (let ((c (call-next-method)))
    (setf (creator c) (creator op))
    c))

(define-change-parser create (class &rest initargs &key n children parent &allow-other-keys)
  (let ((initargs (copy-list initargs)))
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
          inner))))

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

(define-change-parser leave ()
  `(make-instance 'leave-operation))

(defclass tween (change)
  ())

(defgeneric tween-value (tween object clock step))

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

(defmethod copy ((tween slot-tween))
  (let ((c (call-next-method)))
    (setf (slot c) (slot tween))
    c))

(defmethod tick ((tween slot-tween) object clock step)
  (setf (slot-value object (slot tween))
        (tween-value tween object clock step)))

(defclass accessor-tween (tween)
  ((accessor :initarg :accessor :accessor accessor)
   (originals :initform (make-hash-table :test 'eq) :accessor originals)))

(defmethod original-value (object (tween accessor-tween))
  (or (gethash object (originals tween))
      (setf (gethash object (originals tween))
            (funcall (fdefinition (accessor tween)) object))))

(defmethod reset ((tween accessor-tween))
  (loop for object being the hash-keys of (originals tween)
        for value being the hash-values of (originals tween)
        do (funcall (fdefinition `(setf ,(accessor tween))) value object)
           (remhash object (originals tween))))

(defmethod copy ((tween accessor-tween))
  (let ((c (call-next-method)))
    (setf (accessor c) (accessor tween))
    c))

(defmethod tick ((tween accessor-tween) object clock step)
  (funcall (fdefinition `(setf ,(accessor tween)))
           (tween-value tween object clock step) object))

(defclass range-tween (tween)
  ((from :initarg :from :accessor from)
   (to :initarg :to :accessor to)
   (ease-func :initarg :ease :accessor ease-func))
  (:default-initargs
   :from 0
   :to 1
   :ease 'linear))

(defmethod copy ((tween range-tween))
  (let ((c (call-next-method)))
    (setf (from c) (from tween))
    (setf (to c) (to tween))
    (setf (ease-func c) (ease-func tween))
    c))

(defmethod tween-value ((tween range-tween) object clock step)
  (ease-object (or (from tween)
                   (original-value object tween))
               (to tween) step (ease-func tween)))

(defclass constant-tween (tween)
  ((by :initarg :by :accessor by)
   (for :initarg :for :accessor for)
   (start :initform NIL :accessor start))
  (:default-initargs
   :by 1
   :for 1))

(defmethod copy ((tween constant-tween))
  (let ((c (call-next-method)))
    (setf (by c) (by tween))
    (setf (for c) (for tween))
    c))

(defmethod reset :after ((tween constant-tween))
  (setf (start tween) NIL))

(defmethod tween-value ((tween constant-tween) object clock step)
  (let ((rlclock (- clock (or (start tween) (setf (start tween) clock)))))
    (+ (original-value object tween) (* (by tween) (/ rlclock (for tween))))))

(defclass range-slot-tween (range-tween slot-tween)
  ())

(defclass increase-slot-tween (constant-tween slot-tween)
  ())

(defclass range-accessor-tween (range-tween accessor-tween)
  ())

(defclass increase-accessor-tween (constant-tween accessor-tween)
  ())

(define-change-parser set (accessor &key (from 0) (to 1) (ease 'linear))
  `(make-instance 'range-accessor-tween :ease ',ease :from ,from :to ,to :accessor ',accessor))

(define-change-parser increase (accessor &key (by 1) (for 1))
  `(make-instance 'increase-accessor-tween :by ,by :for ,for :accessor ',accessor))

(defclass call-slot-tween (slot-tween call-change)
  ())

(defmethod tween-value ((tween call-slot-tween) object clock step)
  (funcall (func tween) object clock step))

(defclass call-accessor-tween (accessor-tween call-change)
  ())

(defmethod tween-value ((tween call-accessor-tween) object clock step)
  (funcall (func tween) object clock step))

(define-change-parser calc (accessor &key to)
  `(make-instance 'call-accessor-tween
                  :func (lambda (object clock step)
                          (declare (ignorable object clock step))
                          ,to)
                  :accessor ',accessor))
