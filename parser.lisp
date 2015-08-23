#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(deftype interval-designator ()
  `(or real (eql T) (eql NIL)))

(defun designator-p (thing)
  (typep thing 'interval-designator))

(defun parse-intervals (forms)
  (unless (designator-p (first forms))
    (error "~a expected, but ~s found." 'interval-designator (first forms)))
  (dolist (item forms)
    (unless (typep item '(or cons interval-designator))
      (error "~a or ~a expected, but ~s found." 'interval-designator 'cons item)))
  ;; Push boundary to round it off
  (setf (cdr (last forms)) (cons T NIL))
  ;; Normalise
  (let ((normalized ()))
    (loop with start = '?
          with end = '?
          with anims = ()
          for sub = (cdr forms) then (cdr sub)
          for cur = (first forms) then next
          for next = (car sub)
          while (or sub cur)
          do (cond ((and (listp cur) (listp next))
                    (push cur anims))
                   ((listp cur)
                    (push cur anims)
                    (push (list start end (reverse anims)) normalized)
                    (setf start '? end '? anims ()))
                   ((and (designator-p cur) (designator-p next))
                    (setf start cur))
                   ((and (designator-p cur) (not (eql start '?)))
                    (setf end cur))
                   ((and (designator-p cur) (eql start '?))
                    (setf start cur)
                    (setf end (loop for item in (cdr sub)
                                    when (designator-p item)
                                    return item)))))
    (setf normalized (reverse normalized))
    (let ((animations ()))
      (loop for (start end anims) in normalized
            do (dolist (anim anims)
                 (push (list start (if (and (realp start) (realp end))
                                       (- end start) end) anim) animations)))
      (reverse animations))))

(defvar *mapper*)
(defvar *i* 0)

(defun compile-constraint (constraint next)
  (cond ((keywordp constraint)
         (lambda (collective)
           (let ((unit (unit constraint collective)))
             (when unit
               (funcall next unit)))))
        ((realp constraint)
         (lambda (object)
           (when (= 0 (mod *i* constraint))
             (funcall next object))))
        ((eql constraint T)
         (lambda (object)
           (funcall next object)))
        ((symbolp constraint)
         (cond ((string= constraint ">")
                (lambda (container)
                  (do-set (*i* unit) (objects container)
                    (funcall next unit))))
               ((string= constraint "*")
                (lambda (container)
                  (let ((*i* 0))
                    (do-container-tree (unit container)
                      (incf *i*)
                      (funcall next unit)))))
               (T (lambda (object)
                    (when (typep object constraint)
                      (funcall next object))))))
        ((functionp constraint)
         (lambda (object)
           (when (funcall constraint object)
             (funcall next object))))
        ((listp constraint)
         (ecase (first constraint)
           (quote (second constraint))
           (function (fdefinition (second constraint)))
           (lambda (compile NIL constraint))))
        (T (error "Unknown constraint ~s" constraint))))

(defun compile-selector (selector)
  (unless (listp selector)
    (setf selector (list selector)))
  (loop with func = (lambda (set)
                      (funcall *mapper* set))
        for constraint in (reverse selector)
        do (setf func (compile-constraint constraint func))
        finally (return (lambda (collective function)
                          (let ((*mapper* function))
                            (funcall func collective))))))

(defun parse-animation (start duration form)
  (destructuring-bind (selector &rest changes) form
    (let ((animation (gensym "ANIMATION")))
      `(let ((,animation (make-instance 'animation :start ,start :duration ,duration :selector ',selector)))
         ,@(loop for change in changes
                 collect `(push ,(parse-change change) (changes ,animation)))
         ,animation))))

(defun nenter (thing target)
  (if (consp target)
      (setf (cdr target) (cons thing (cdr target)))
      (enter thing target)))

(defun parse-change (form)
  (cond ((subtypep (first form) 'tween)
         `(make-instance ',(first form) :field ',(second form) ,@(cddr form)))
        (T
         ;; Generalise.
         (ecase (first form)
           (create
            (labels ((process-contents (parent contents)
                       (destructuring-bind (name &rest initargs &key contents (count 1) &allow-other-keys) contents
                         (remf initargs :contents)
                         (remf initargs :count)
                         (let ((inner `(nenter ,(if contents
                                                    (let ((parent (gensym "ENTITY")))
                                                      `(let ((,parent (make-instance ',name ,@initargs)))
                                                         ,(process-contents parent contents)))
                                                    `(make-instance ',name ,@initargs))
                                               ,parent)))
                           (case count
                             ((0 NIL))
                             (1 inner)
                             (T `(dotimes (,(gensym "I") ,count ,parent)
                                   ,inner)))))))
              (let ((entities (gensym "ENTITIES")))
                `(let ((,entities (funcall 'list NIL)))
                   ,(process-contents entities (copy-list (cdr form)))
                   (cdr ,entities)))))
           (leave
            `(make-instance 'leave))
           (enter
            `(make-instance 'enter :object (lambda () (first ,(parse-change `(create ,@(cdr form)))))))
           (every
            `(make-instance 'every. :distance ,(second form) :change ,(parse-change (third form))))))))

(defmacro make-progression (name &body intervals)
  (let ((progression (gensym "PROGRESSION")))
    `(let ((,progression (make-instance 'progression :name ',name)))
       ,@(loop for (start duration form) in (parse-intervals intervals)
               collect `(enter ,(parse-animation start duration form) ,progression))
       ,progression)))

(defvar *progressions* (make-hash-table :test 'eql))

(defun progression-definition (name)
  (gethash name *progressions*))

(defun (setf progression-definition) (progression name)
  (setf (gethash name *progressions*) progression))

(defun remove-progression (name)
  (remhash name *progressions*))

(defmacro define-progression (name &body intervals)
  (let ((progression (gensym "PROGRESSION")))
    `(let ((,progression
             (or (progression-definition ',name)
                 (setf (progression-definition ',name)
                       (make-instance 'progression :name ',name)))))
       (clear ,progression)
       ,@(loop for (start duration form) in (parse-intervals intervals)
               collect `(enter ,(parse-animation start duration form) ,progression))
       ',name)))
