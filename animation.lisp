#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defclass progression () ())
(defclass animation () ())
(defgeneric tick (progression animation scene))
(defgeneric start-animation (animation progression))
(defgeneric animations (progression))
(defgeneric animation (n progression))
(defgeneric duration (animation))
(defgeneric selector (animation))
(defgeneric changes (animation))

(define-self-returning-method tick (progression animation scene))
(define-self-returning-method start-animation (animation progression))

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

(defmethod reset ((progression progression))
  (call-next-method)
  (map NIL #'reset (animations progression))
  (setf (active-set progression) ())
  (setf (active-pointer progression) 0))

(defmethod clear ((progression progression))
  (setf (active-set progression) ())
  (setf (active-pointer progression) 0)
  (setf (animations progression) (make-array 0)))

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
    (stop progression)))

(defmethod tick ((progression progression) (animation (eql T)) scene)
  (update progression)
  (dolist (active (active-set progression))
    ;; (v:info :test "TICKING ~a" active)
    (tick progression (cdr active) scene)))

(defmethod enter ((animation animation) (progression progression))
  (let ((new-els (stable-sort (cons animation (coerce (animations progression) 'list))
                       #'< :key (lambda (a)
                                  (if (realp (start a)) (start a) most-positive-fixnum)))))
    (setf (animations progression)
          (coerce new-els 'vector))
    (setf (active-pointer progression) 0)))

(defmethod leave ((animation animation) (progression progression))
  (let ((new-els (remove animation (coerce (animations progression) 'list))))
    (setf (animations progression)
          (coerce new-els 'vector))
    (setf (active-pointer progression) 0)))

(defmethod animation (n (progression progression))
  (aref (animations progression) n))

(defmethod start-animation ((animation animation) (progression progression))
  (v:info :test "STARTING: ~a" animation)
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

(defmethod tick ((progression progression) (animation animation) scene)
  (let* ((clock (clock progression))
         (step (unless (or (eql t (duration animation))
                           (= 0 (duration animation)))
                 (/ (- clock (start animation)) (duration animation)))))
    ;; This is shit. FIXME
    (dolist (change (changes animation))
      (unless (done change)
        (funcall (selector animation)
                 scene (lambda (object)
                         (perform change object clock step)))))))
