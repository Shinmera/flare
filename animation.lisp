#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defclass animatable () ())
(defclass progression-definition () ())
(defclass progression () ())
(defclass animation () ())
(defclass change () ())

(defgeneric progressions (animatable))
(defgeneric add-progression (progression animatable))
(defgeneric remove-progression (progression animatable))
(defgeneric progression (denominator animatable))

(defgeneric animations (progression-definition))
(defgeneric instances (progression-definition))
(defgeneric progression-instance (progression-definition))

(defgeneric present-animations (progression))
(defgeneric past-animations (progression))
(defgeneric future-animations (progression))

(defgeneric copy (animation))
(defgeneric beginning (animation))
(defgeneric duration (animation))
(defgeneric changes (animation))
(defgeneric selector (animation))
(defgeneric tick (animation animatable clock step))

(defclass animatable ()
  ((progressions :initform () :accessor progressions)))

(defmethod update :after ((animatable animatable))
  (dolist (progression (progressions animatable))
    (update progression)))

(defmethod reset :before ((animatable animatable))
  (dolist (progression (progressions animatable))
    (reset progression)))

(defmethod add-progression ((progression progression) (animatable animatable))
  (when (and (animatable progression)
             (not (eql (animatable progression) animatable)))
    (error ""))
  (setf (animatable progression) animatable)
  (push progression (progressions animatable))
  progression)

(defmethod enter ((progression progression) (animatable animatable))
  (add-progression progression animatable))

(defmethod remove-progression ((progression progression) (animatable animatable))
  (unless (eql (animatable progression) animatable)
    (error ""))
  (setf (animatable progression) NIL)
  (setf (progressions animatable)
        (delete progression (progressions animatable)))
  progression)

(defmethod leave ((progression progression) (animatable animatable))
  (remove-progression progression animatable))

(defmethod progression ((definition progression-definition) (animatable animatable))
  (loop for progression in (progressions animatable)
        when (eql (definition progression) definition)
        collect progression))

(defclass progression-definition ()
  ((animations :initform (make-array 0) :accessor animations)
   (instances :initform () :accessor instances)))

(defmethod progression-instance ((definition progression-definition))
  (let ((instance (make-instance 'progression :definition definition)))
    (push (tg:make-weak-pointer instance) (instances definition))
    instance))

(defmethod add-progression ((definition progression-definition) (animatable animatable))
  (add-progression (progression-instance definition) animatable))

(defmethod enter ((definition progression-definition) (animatable animatable))
  (add-progression definition animatable))

(defmethod (setf animations) (animations (definition progression-definition))
  (setf (slot-value definition 'animations)
        (ensure-sorted animations #'< :key #'start)))

(defmethod (setf animations) :after (val (definition progression-definition))
  ;; Take the chance to clear out empty references.
  (setf (instances definition) (delete-if-not #'tg:weak-pointer-value (instances definition)))
  (loop for pointer in (instances definition)
        do (setf (animations (tg:weak-pointer-value pointer)) (animations definition))))

(defclass progression (clock)
  ((definition :initarg :definition :accessor definition)
   (animatable :initarg :animatable :accessor animatable)
   (active :initform #() :accessor present-animations)
   (ended :initform #() :accessor past-animations)
   (future :initform #() :accessor future-animations))
  (:default-initargs
   :animatable NIL
   :definition (error "DEFINITION required.")))

(defmethod initialize-instance :after ((progression progression) &key)
  (setf (animations progression) (animations (definition progression))))

(defmethod print-object ((progression progression) stream)
  (print-unreadable-object (progression stream :type T :identity T)
    (format stream "~s ~s ~s ~s"
            (if (running progression) :started :stopped) (clock progression)
            :animatable (animatable progression))))

(defun copy-animations (thing)
  (let ((new (make-array (length thing) :fill-pointer (length thing))))
    (etypecase thing
      (vector (loop for i from 0 below (length thing)
                    do (setf (aref new i) (copy (aref thing i)))))
      (list (loop for i from 0
                  for el in thing
                  do (setf (aref new i) (copy el)))))
    new))

(defmethod (setf animations) (animations (progression progression))
  (let ((clock (clock progression)))
    ;; Rewind
    (reset progression)
    ;; Unload new changes
    (setf (future-animations progression)
          (ensure-sorted (copy-animations animations) #'< :key #'start))
    (setf (present-animations progression)
          (make-array (length (future-animations progression)) :fill-pointer 0))
    (setf (past-animations progression)
          (make-array (length (future-animations progression)) :fill-pointer 0))
    ;; Fast-forward
    (setf (clock progression) clock)
    (cond ((running progression)
           (update progression))
          (T
           (setf (running progression) T)
           (update progression)
           (setf (running progression) NIL))))
  animations)

(defvar *resetting* NIL) ; oh dear.

(defmethod reset ((progression progression))
  (let ((*resetting* T))
    ;; Rewind done changes to active set
    (loop repeat (length (past-animations progression))
          do (vector-push (vector-pop (past-animations progression))
                          (present-animations progression)))
    ;; Resort to ascertain order of activation
    (setf (present-animations progression)
          (ensure-sorted (present-animations progression) #'> :key #'start))
    ;; Reset in order.
    (loop for animation across (present-animations progression)
          do (reset animation))
    (loop repeat (length (present-animations progression))
          for animation = (vector-pop (present-animations progression))
          do (vector-push animation (future-animations progression)))
    ;; Fix clock.
    (call-next-method))
  progression)

(defun shift-array-elements (from to test)
  (loop with i = 0
        while (< i (length from))
        do (cond ((funcall test (aref from i))
                  (vector-push (array-utils:vector-pop-position from i) to))
                 (T
                  (incf i)))))

;; Trix! This is called automatically on an UPDATE due to the
;; inheritance from the CLOCK calling it.
(defmethod (setf clock) :before (new (progression progression))
  ;; If we're travelling backwards we first need to reset completely.
  (let ((old (clock progression)))
    (when (and (< new old) (not *resetting*))
      (reset progression))))

(defmethod update ((progression progression))
  ;; Start new ones
  (shift-array-elements
   (future-animations progression)
   (present-animations progression)
   (lambda (animation)
     (<= (beginning animation) (clock progression))))
  ;; Animate
  (when (animatable progression)
    (loop for animation across (present-animations progression)
          for step = (cond ((eql T (duration animation))
                            T)
                           ((<= (duration animation) 0)
                            1.0)
                           (T
                            (min (/ (- (clock progression) (beginning animation))
                                    (duration animation))
                                 1.0)))
          do (tick animation (animatable progression) (clock progression) step)))  
  ;; End expired
  (shift-array-elements
   (present-animations progression)
   (past-animations progression)
   (lambda (animation)
     (and (not (eql (duration animation) T))
          (<= (+ (beginning animation) (duration animation)) (clock progression)))))
  ;; Stop altogether if finished
  (when (= 0
           (length (present-animations progression))
           (length (future-animations progression)))
    (stop progression)))

(defclass animation ()
  ((beginning :initarg :beginning :accessor beginning)
   (duration :initarg :duration :accessor duration)
   (selector :initarg :selector :accessor selector)
   (changes :initarg :changes :accessor changes))
  (:default-initargs
   :start (error "BEGINNING needed.")
   :duration (error "DURATION needed.")
   :selector T
   :changes ()))

(defmethod initialize-instance :after ((animation animation) &key)
  (setf (selector animation) (selector animation)))

(defmethod print-object ((animation animation) stream)
  (print-unreadable-object (animation stream :type T :identity T)
    (format stream "~s ~s ~s ~s" :start (beginning animation) :duration (duration animation))))

(defmethod (setf selector) (value (animation animation))
  (setf (slot-value animation 'selector)
        (typecase value
          (function value)
          (T (compile-selector value)))))

(defmethod copy ((animation animation))
  (make-instance 'animation
                 :start (beginning animation)
                 :duration (duration animation)
                 :selector (selector animation)
                 :changes (mapcar #'copy (changes animation))))

(defmethod tick ((animation animation) (animatable animatable) clock step)
  (funcall (selector animation)
           animatable
           (lambda (object)
             (dolist (change (changes animation))
               (tick change object clock step)))))

(defmethod reset ((animation animation))
  (dolist (change (changes animation))
    (reset change)))


(defun format-progression (progr)
  (format T "~&Clock: ~a~
             ~&
             ~&Progression:~
             ~&  Future:~{~
             ~&    ~a~}
             ~&  Present:~{~
             ~&    ~a~}~
             ~&  Past: ~{~
             ~&    ~a~}~
             ~&
             ~&Scene:"
          (clock (animatable progr))
          (coerce (future-animations progr) 'list)
          (coerce (present-animations progr) 'list)
          (coerce (past-animations progr) 'list))
  (print-container-tree (animatable progr)))


(defun simulate-progression (def)
  (let ((scene (make-instance 'scene))
        (progr (progression-instance def)))
    (enter progr scene)
    (start scene)
    (start progr)
    (loop (update scene)
          (format-progression progr)
          (sleep 0.7))))
