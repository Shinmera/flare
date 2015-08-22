#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric tick (progression scene))
(defgeneric add-animation (animation progression))
(defgeneric remove-animation (animation progression))
(defgeneric compile-change (name args))
(defgeneric perform (change object clock step))
(defgeneric initial-value (tween object))
(defgeneric animations (progression))
(defgeneric animation (n progression))
(defgeneric start (animation))
(defgeneric end (animation))
(defgeneric selector (animation))
(defgeneric changes (animation))
(defgeneric field (tween))
(defgeneric from (tween))
(defgeneric to (tween))
(defgeneric from (tween))
(defgeneric by (tween))
(defgeneric ease-func (tween))

(defclass progression ()
  ((animations :initform (make-array 0) :accessor animations)
   (active-set :initform () :accessor active-set)
   (active-pointer :initform 0 :accessor active-pointer)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod print-object ((progression progression) stream)
  (print-unreadable-object (progression stream :type T)
    (format stream "~s ~s" :name (name progression))))

(defmethod tick ((progression progression) scene)
  (let ((clock (clock scene)))
    ;; Filter expired
    (setf (active-set progression)
          (remove-if (lambda (a) (and (not (eql T (end a))) (<= (end a) clock)))
                     (active-set progression)))
    ;; Pop new
    (loop while (< (active-pointer progression) (length (animations progression)))
          for animation = (aref (animations progression) (active-pointer progression))
          while (<= (start animation) clock)
          do (incf (active-pointer progression))
             (unless (find animation (active-set progression))
               (push animation (active-set progression))))
    ;; Process active
    (dolist (animation (active-set progression))
      (tick animation scene))))

(defmethod add-animation (animation (progression progression))
  (let ((new-els (sort (cons animation (coerce (animations progression) 'list))
                       #'< :key #'start)))
    (setf (animations progression)
          (coerce new-els 'vector))
    (setf (active-pointer progression) 0))
  animation)

(defmethod remove-animation (animation (progression progression))
  (let ((new-els (remove animation (coerce (animations progression) 'list))))
    (setf (animations progression)
          (coerce new-els 'vector))
    (setf (active-pointer progression) 0))
  animation)

(defmethod animation (n (progression progression))
  (aref (animations progression) n))

(defclass animation ()
  ((start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (selector :initarg :selector :accessor selector)
   (changes :initarg :changes :accessor changes))
  (:default-initargs
   :start 0 :end T :selector T :changes ()))

(defmethod initialize-instance :after ((animation animation) &key)
  (setf (selector animation) (selector animation)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type T)
    (format stream "~s ~s ~s ~s" :start (start animation) :end (end animation))))

(defmethod (setf selector) :after (selector (animation animation))
  (unless (functionp (selector animation))
    (setf (selector animation) (compile-selector (selector animation)))))

(defmethod tick ((animation animation) scene)
  (let* ((clock (clock scene))
         (step (if (eql t (end animation))
                   NIL
                   (/ (- clock (start animation))
                      (- (end animation) (start animation))))))
    (funcall (selector animation)
             scene (lambda (object)
                     (dolist (change (changes animation))
                       (perform change object clock step))))))

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
