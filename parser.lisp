#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defun parse-intervals (forms)
  (unless (realp (first forms))
    (error "First statement must be a number."))
  (let ((animations ()))
    (loop with begin = (first forms)
          with temps = ()
          for statement in (rest forms)
          do (etypecase statement
               (list (push statement temps))
               (real
                (dolist (form (reverse temps))
                  (push (list begin statement form) animations))
                (setf temps ())
                (setf begin statement)))
          finally (dolist (form temps)
                    (push (list begin T form) animations)))
    (reverse animations)))

(defvar *mapper*)
(defvar *i* 0)

(defun compile-constraint (constraint next)
  (cond ((keywordp constraint)
         (lambda (collective)
           (let ((unit (unit constraint collective)))
             (when unit
               (funcall next unit)))))
        ((eql constraint '>)
         (lambda (container)
           (do-set (*i* unit) (objects container)
             (funcall next unit))))
        ((realp constraint)
         (lambda (object)
           (when (= 0 (mod *i* constraint))
             (funcall next object))))
        ((eql constraint T)
         (lambda (object)
           (funcall next object)))
        ((symbolp constraint)
         (lambda (object)
           (when (typep object constraint)
             (funcall next object))))
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

(defun parse-change (form)
  `(compile-change ',(car form) ',(cdr form)))

(defun parse-animation (start end form)
  (destructuring-bind (selector &rest changes) form
    (let ((animation (gensym "ANIMATION")))
      `(let ((,animation (make-instance 'animation :start ,start :end ,end :selector ',selector)))
         ,@(loop for change in changes
                 collect `(push ,(parse-change change) (changes ,animation)))
         ,animation))))

(defmacro define-progression (name &body intervals)
  (let ((defs (parse-intervals intervals))
        (progression (gensym "PROGRESSION")))
    `(let ((,progression (make-instance 'progression :name ',name)))
       ,@(loop for (start end form) in defs
               collect `(enqueue ,(parse-animation start end form) ,progression)))))
