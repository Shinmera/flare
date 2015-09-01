#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defclass animatable () ())
(defclass progression () ())
(defclass animation () ())
(defgeneric progressions (animatable))
(defgeneric progression (name animatable))
(defgeneric tick (progression animation animatable))
(defgeneric start-animation (animation progression))
(defgeneric animations (progression))
(defgeneric animation (n progression))
(defgeneric duration (animation))
(defgeneric selector (animation))
(defgeneric changes (animation))

(define-self-returning-method tick (progression animation animatable))
(define-self-returning-method start-animation (animation progression))

(defclass animatable ()
  ((progressions :initform (make-hash-table :test 'eql) :accessor progressions)))

(defmethod enter ((def progression-definition) (animatable animatable))
  (enter (make-progression def) animatable))

(defmethod enter ((progression progression) (animatable animatable))
  (when (and (animatable progression)
             (not (eql (animatable progression) animatable)))
    (warn "~a must leave ~a to enter ~a."
          progression (animatable progression) animatable)
    (leave progression T))
  (setf (gethash (name progression) (progressions animatable)) progression)
  (setf (animatable progression) animatable))

(defmethod leave ((progression progression) (animatable animatable))
  (unless (eql (animatable progression) animatable)
    (error "~a cannot leave ~a as it is a progression of ~a."
           progression animatable (animatable progression)))
  (reset progression)
  (setf (gethash (name progression) (progressions animatable)) NIL)
  (setf (animatable progression) NIL))

(defmethod progression (name ((animatable animatable)))
  (gethash name (progressions animatable)))

(defmethod update :before ((animatable animatable))
  (loop for progression being the hash-values of (progressions animatable)
        do (update progression)))

(defclass progression-definition ()
  ((animations :initform (make-array 0) :accessor animations)
   (name :initarg :name :accessor name)
   (instances :initform () :accessor instances)))

(defmethod make-progression ((def progression-definition))
  (let ((in (make-instance 'progression :definition def)))
    (push (tg:make-weak-pointer in) (instances def))
    in))

(defmethod (setf animations) :before (an (def progression-definition))
  (loop for pointer in (instances def)
        for instance = (tg:weak-pointer-value pointer)
        when instance
        do (reset instance)))

(defmethod enter ((animation animation) (def progression-definition))
  (let ((new-els (stable-sort (cons animation (coerce (animations progression) 'list))
                              #'< :key (lambda (a)
                                         (if (realp (start a)) (start a) most-positive-fixnum)))))
    (setf (animations progression)
          (coerce new-els 'vector))))

(defmethod leave ((animation animation) (def progression-definition))
  (let ((new-els (remove animation (coerce (animations progression) 'list))))
    (setf (animations progression)
          (coerce new-els 'vector))))

(defmethod clear ((def progression-definition))
  (setf (animations def) (make-array 0))
  (setf (instances def) ()))

(defclass progression (clock)
  ((active-set :initform () :accessor active-set)
   (active-pointer :initform 0 :accessor active-pointer)
   (definition :initarg :definition :accessor definition)
   (animatable :initarg :animatable :accessor animatable))
  (:default-initargs
   :definition (error "DEFINITION required.")
   :animatable NIL))

(defmethod print-object ((progression progression) stream)
  (print-unreadable-object (progression stream :type T)
    (format stream "~s ~s ~a" :name (name progression) (clock progression))))

(defmethod describe-object ((progression progression) stream)
  (format stream "~&~a
  [~a]

Name: ~s
The clock is ~:[STOPPED~;RUNNING~]
Internal clock is at ~a
#Active animations: ~d
Animations:
  ~{~a~^~%  ~}~&"
          progression (type-of progression) (name progression)
          (running progression) (clock progression) (length (active-set progression))
          (coerce (animations progression) 'list)))

(defmethod animations ((progression progression))
  (animations (definition progression)))

(defmethod name ((progression progression))
  (name (definition progression)))

(defmethod leave ((progression progression) (where (eql T)))
  (when (animatable progression)
    (leave progression (animatable progression))))

(defmethod reset ((progression progression))
  (call-next-method)
  (map NIL #'reset (animations progression))
  (setf (active-set progression) ())
  (setf (active-pointer progression) 0))

(defmethod clear ((progression progression))
  (setf (active-set progression) ())
  (setf (active-pointer progression) 0))

(defmethod update ((progression progression))
  ;; Filter expired
  (setf (active-set progression)
        (remove-if (lambda (a) (and (not (eql T (duration (cdr a))))
                                    (<= (+ (car a) (duration (cdr a))) (clock progression))))
                   (active-set progression)))
  ;; Start new
  (loop while (< (active-pointer progression) (length (animations progression)))
        for animation = (aref (animations progression) (active-pointer progression))
        for start = (start animation)
        while (or (eql start NIL) (<= start (clock progression)))
        do (unless (or (eql start NIL) (find animation (active-set progression)))
             (start-animation animation progression)))
  ;; Are we done?
  (when (and (= (active-pointer progression) (length (animations progression)))
             (not (active-set progression)))
    (stop progression))
  ;; Ok, update things.
  (dolist (active (active-set progression))
    (tick progression (cdr active) (animatable progression))))

(defmethod animation (n (progression progression))
  (aref (animations progression) n))

(defmethod start-animation ((animation animation) (progression progression))
  (when (eq animation (aref (animations progression) (active-pointer progression)))
    (incf (active-pointer progression)))
  (push (cons (clock progression) animation) (active-set progression)))

(defclass animation ()
  ((start :initarg :start :accessor start)
   (duration :initarg :duration :accessor duration)
   (selector :initarg :selector :accessor selector)
   (changes :initarg :changes :accessor changes))
  (:default-initargs
   :start 0 :duration T :selector T :changes ()))

(defmethod initialize-instance :after ((animation animation) &key)
  (setf (selector animation) (selector animation)))

(defmethod describe-object ((animation animation) stream)
  (format stream "~&~a
  [~a]

Start: ~a
Duration: ~a
Changes:
  ~{~a~^~%  ~}~&"
          animation (type-of animation)
          (start animation) (duration animation) (changes animation)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type T)
    (format stream "~s ~s ~s ~s" :start (start animation) :duration (duration animation))))

(defmethod (setf selector) :after (selector (animation animation))
  (unless (functionp (selector animation))
    (setf (selector animation) (compile-selector (selector animation)))))

(defmethod reset ((animation animation))
  (dolist (change (changes animation))
    (reset change)))

(defmethod tick ((progression progression) (animation animation) animatable)
  (let* ((clock (clock progression))
         (step (unless (or (eql t (duration animation))
                           (= 0 (duration animation)))
                 (/ (- clock (start animation)) (duration animation)))))
    ;; This is shit. FIXME
    (dolist (change (changes animation))
      (unless (done change)
        (funcall (selector animation)
                 animatable (lambda (object)
                         (perform change object clock step)))))))
