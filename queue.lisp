#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.queue)

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

(defclass queue ()
  ((head :initform (make-cell NIL NIL NIL) :accessor head)
   (tail :initform (make-cell NIL NIL NIL) :accessor tail)
   (size :initform 0 :reader size :writer set-size)))

(defmethod initialize-instance :after ((queue queue) &key)
  (setf (right (head queue)) (tail queue)
        (left (head queue)) (head queue))
  (setf (left (tail queue)) (head queue)
        (right (tail queue)) (tail queue))
  queue)

(defun make-queue ()
  (make-instance 'queue))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro loop-queue ((current) queue &body loop-clauses)
    (let ((next (gensym "NEXT"))
          (cqueue (gensym "SET")))
      `(loop with ,cqueue = ,queue
             for ,current = (right (head ,cqueue)) then ,next
             for ,next = (right ,current)
             until (eq ,current (tail ,cqueue))
             ,@loop-clauses))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-queue (function queue)
    (loop-queue (current) queue
                for i from 0
                do (funcall function i (value current)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-queue ((index value &optional result) queue &body body)
    `(block NIL
       (map-queue (lambda (,index ,value) ,@body) ,queue)
       ,result)))

(defun enqueue (value queue)
  (cell-insert-before (make-cell value NIL NIL) (tail queue))
  (set-size (1+ (size queue)) queue)
  queue)

(defun dequeue (queue)
  (if (eql (right (head queue)) (tail queue))
      (values NIL NIL)
      (let ((cell (right (head queue))))
        (cell-remove cell)
        (set-size (1- (size queue)) queue)
        (values (value cell) T))))

(defun queue-remove (value queue)
  (loop-queue (current) queue
              do (when (eql (value current) value)
                   (cell-remove current)
                   (return T))))

(defun queue-size (queue)
  (size queue))

(defun queue-first (queue)
  (values (value (right (head queue)))
          (not (eql (right (head queue)) (tail queue)))))

(defun queue-last (queue)
  (value (left (tail queue))
         (not (eql (left (tail queue)) (head queue)))))

(defun queue-value-at (n queue)
  (do-queue (i val) queue
    (when (= i n)
      (return val))))

(defun queue-index-of (value queue)
  (do-queue (i val) queue
    (when (eql val value)
      (return i))))

(defun clear-queue (queue)
  (setf (left (tail queue)) (head queue)
        (right (head queue)) (tail queue))
  queue)

(defun in-queue-p (value queue)
  (do-queue (i val) queue
    (when (eql val value)
      (return T))))

(defun coerce-queue (queue type)
  (ecase type
    (queue
     queue)
    (list
     (loop-queue (current) queue
       collect (value current)))
    (vector
     (let ((vec (make-array (size queue))))
       (loop-queue (current) queue
         for i from 0
         do (setf (aref vec i) (value current)))
       vec))
    (sequence
     (coerce-queue queue 'list))))



