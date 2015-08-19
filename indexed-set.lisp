#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.indexed-set)

(defstruct (cell (:conc-name NIL)
                 (:constructor make-cell (value left right))
                 (:copier NIL)
                 (:predicate NIL))
  value left right)

(defun cell-insert-before (cell neighbor)
  (setf (left cell) (left neighbor)
        (right cell) neighbor
        (right (left cell)) cell
        (left neighbor) cell)
  cell)

(defun cell-remove (cell)
  (setf (left (right cell)) (left cell)
        (right (left cell)) (right cell))
  cell)

(defclass indexed-set ()
  ((head :initform (make-cell NIL NIL NIL) :accessor head)
   (tail :initform (make-cell NIL NIL NIL) :accessor tail)
   (set :initform (make-hash-table :test 'eql) :accessor set)
   (size :initform 0 :accessor size)))

(defmethod initialize-instance :after ((set indexed-set) &key)
  (setf (right (head set)) (tail set))
  (setf (left (tail set)) (head set))
  set)

(defun make-indexed-set ()
  (make-instance 'indexed-set))

(defun set-add (value set)
  (cond ((gethash value (set set))
         (values set NIL))
        (T
         (let ((cell (make-cell value NIL NIL)))
           (setf (gethash value (set set)) cell)
           (cell-insert-before cell (tail set)))
         (incf (size set))
         (values set T))))

(defun set-remove (value set)
  (let ((cell (gethash value (set set))))
    (cond (cell
           (remhash value (set set))
           (cell-remove cell)
           (decf (size set))
           (values set T))
          (T
           (values set NIL)))))

(defun in-set-p (value set)
  (nth-value 1 (gethash value (set set))))

(defun set-size (set)
  (size set))

(defun set-clear (set)
  (setf (left (tail set)) (head set)
        (right (head set)) (tail set))
  set)

(defun set-first (set)
  (value (right (head set))))

(defun set-last (set)
  (value (left (tail set))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro loop-set ((current) set &body loop-clauses)
    (let ((next (gensym "NEXT"))
          (cset (gensym "SET")))
      `(loop with ,cset = ,set
             for ,current = (right (head ,cset)) then ,next
             for ,next = (right ,current)
             until (eq ,current (tail ,cset))
             ,@loop-clauses))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-set (function set)
    (loop-set (current) set
      for i from 0
      do (funcall function i (value current)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-set ((index value &optional result) set &body body)
    `(block NIL
       (map-set (lambda (,index ,value) ,@body) ,set)
       ,result)))

(defun set-value-at (n set)
  (do-set (i val) set
    (when (= i n)
      (return val))))

(defun set-index-of (value set)
  (do-set (i val) set
    (when (eql val value)
      (return i))))

(defun coerce-set (set type)
  (ecase type
    (indexed-set
     set)
    (list
     (loop-set (current) set
       collect (value current)))
    (vector
     (let ((vec (make-array (size set))))
       (loop-set (current) set
         for i from 0
         do (setf (aref vec i) (value current)))
       vec))
    (sequence
     (coerce-set set 'list))
    (hash-set
     (let ((table (make-hash-table :test 'eql)))
       (do-set (i val table) set
         (declare (ignore i))
         (setf (gethash val table) val))))))
