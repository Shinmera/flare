#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric tick (progression scene))
(defgeneric add-animation (animation progression))
(defgeneric remove-animation (animation progression))
(defgeneric perform (change object clock step))
(defgeneric initial-value (tween object))
(defgeneric animations (progression))
(defgeneric animation (n progression))
(defgeneric start (animation))
(defgeneric duration (animation))
(defgeneric selector (animation))
(defgeneric changes (animation))

(defclass progression (clock)
  ((animations :initform (make-array 0) :accessor animations)
   (active-set :initform () :accessor active-set)
   (active-pointer :initform 0 :accessor active-pointer)
   (name :initarg :name :accessor name))
  (:default-initargs
   :name (error "NAME required.")))

(defmethod print-object ((progression progression) stream)
  (print-unreadable-object (progression stream :type T)
    (format stream "~s ~s ~a" :name (name progression) (clock progression))))

(defmethod reset ((progression progression))
  (call-next-method)
  (setf (active-set progression) ())
  (setf (active-pointer progression) 0)
  progression)

(defmethod update ((progression progression))
  ;; Filter expired
  (setf (active-set progression)
        (remove-if (lambda (a) (and (not (eql T (duration (cdr a))))
                                    (<= (+ (car a) (duration (cdr a))) (clock progression))))
                   (active-set progression)))
  ;; Pop new
  (loop while (< (active-pointer progression) (length (animations progression)))
        for animation = (aref (animations progression) (active-pointer progression))
        for start = (start animation)
        while (or (eql start NIL) (<= start (clock progression)))
        do (unless (or (eql start NIL) (find animation (active-set progression)))
             (activate-animation animation progression))))

(defmethod tick ((progression progression) scene)
  (update progression)
  (dolist (active (active-set progression))
    (tick (cdr active) scene)))

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

(defmethod activate-animation (animation (progression progression))
  (when (eq animation (aref (animations progression) (active-pointer progression)))
    (incf (active-pointer progression)))
  (push (cons (clock progression) animation) (active-set progression))
  animation)

(defclass animation ()
  ((start :initarg :start :accessor start)
   (duration :initarg :duration :accessor duration)
   (selector :initarg :selector :accessor selector)
   (changes :initarg :changes :accessor changes))
  (:default-initargs
   :start 0 :duration T :selector T :changes ()))

(defmethod initialize-instance :after ((animation animation) &key)
  (setf (selector animation) (selector animation)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type T)
    (format stream "~s ~s ~s ~s" :start (start animation) :duration (duration animation))))

(defmethod (setf selector) :after (selector (animation animation))
  (unless (functionp (selector animation))
    (setf (selector animation) (compile-selector (selector animation)))))

(defmethod tick ((animation animation) scene)
  (let* ((clock (clock scene))
         (step (unless (eql t (duration animation))
                 (/ (- clock (start animation)) (duration animation)))))
    (funcall (selector animation)
             scene (lambda (object)
                     (dolist (change (changes animation))
                       (perform change object clock step))))))
