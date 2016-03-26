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
                 (push (list start
                             (if (and (realp start) (realp end)) (- end start) end)
                             anim)
                       animations)))
      (reverse animations))))

(defvar *mapper*)
(defvar *i* 0)

(defun compile-constraint (constraint next)
  (cond ((keywordp constraint)
         (lambda (collective)
           (let ((unit (unit constraint collective)))
             (when unit
               (funcall next unit)))))
        ((integerp constraint)
         (lambda (object)
           (when (= constraint *i*)
             (funcall next object))))
        ((eql constraint T)
         (lambda (object)
           (funcall next object)))
        ((symbolp constraint)
         (cond ((string= constraint ">")
                (lambda (container)
                  (let ((*i* 0))
                    (do-set (unit (objects container))
                      (funcall next unit)
                      (incf *i*)))))
               ((string= constraint "*")
                (lambda (container)
                  (let ((*i* 0))
                    (do-container-tree (unit container)
                      (funcall next unit)
                      (incf *i*)))))
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

(defmacro compile-change (type &rest args)
  (parse-change type args))

(defun parse-animation (beginning duration expression)
  (destructuring-bind (selector &rest changes) expression
    (let ((animation (gensym "ANIMATION")))
      `(let ((,animation (make-instance 'animation :beginning ,beginning :duration ,duration :selector ',selector)))
         ,@(loop for change in changes
                 collect `(push (compile-change ,@change) (changes ,animation)))
         ,animation))))

(defmacro compile-animations (&body intervals)
  (let ((animations (when intervals (parse-intervals intervals))))
    `(list
      ,@(loop for (start duration expression) in animations
              collect (parse-animation start duration expression)))))

(defvar *progressions* (make-hash-table :test 'eql))

(defun progression-definition (name)
  (gethash name *progressions*))

(defun (setf progression-definition) (progression name)
  (setf (gethash name *progressions*) progression))

(defun remove-progression-definition (name)
  (remhash name *progressions*))

(defmacro define-progression (name &body intervals)
  `(setf (animations (or (progression-definition ',name)
                         (setf (progression-definition ',name)
                               (make-instance 'progression-definition))))
         (compile-animations ,@intervals)))

(defmethod progression-instance ((name symbol))
  (progression-instance (progression-definition name)))
