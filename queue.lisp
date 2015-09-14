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

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type T)
    (format stream "~s ~s" :size (size queue))))

(defun make-queue ()
  (make-instance 'queue))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro-driver (FOR var ON-QUEUE q)
    (let ((queue (gensym "QUEUE"))
          (tail (gensym "TAIL"))
          (kwd (if generate 'generate 'for)))
      `(progn
         (with ,queue = ,q)
         (with ,tail = (tail ,queue))
         (initially (setf ,var (head ,queue)))
         (,kwd ,var next (right ,var))
         (until (eq ,var ,tail)))))
  
  (defmacro-driver (FOR var IN-QUEUE q)
    (let ((kwd (if generate 'generate 'for))
          (cell (gensym "CELL")))
      `(progn
         (,kwd ,cell on-queue ,q)
         (,kwd ,var next (value ,cell))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-queue (function queue)
    (iterate
      (for var in-queue queue)
      (funcall function var))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro do-queue ((value queue &optional result) &body body)
    `(block NIL
       (map-queue (lambda (,value) ,@body) ,queue)
       ,result)))

(defun enqueue (value queue)
  (cell-insert-before (make-cell value NIL NIL) (tail queue))
  (set-size (1+ (size queue)) queue)
  queue)

(defun dequeue (queue)
  ;; The sentinel would avoid this check usually
  ;; but we need to keep the counter intact, and
  ;; having a secondary value to tell us whether
  ;; it is empty is also useful, so we need to test.
  (if (eql (right (head queue)) (tail queue))
      (values NIL NIL)
      (let ((cell (right (head queue))))
        (cell-remove cell)
        (set-size (1- (size queue)) queue)
        (values (value cell) T))))

(defun queue-remove (value queue)
  (iterate
    (for cell on-queue queue)
    (when (eql (value cell) value)
      (cell-remove cell)
      (return T))))

(defun queue-size (queue)
  (size queue))

(defun queue-first (queue)
  (values (value (right (head queue)))
          (not (eql (right (head queue)) (tail queue)))))

(defun queue-last (queue)
  (values (left (tail queue))
          (not (eql (left (tail queue)) (head queue)))))

(defun queue-value-at (n queue)
  (iterate
    (for current in-queue queue)
    (for i from 0)
    (when (= i n)
      (return (values current T))))
  (values NIL NIL))

(defun queue-index-of (value queue)
  (iterate
    (for current in-queue queue)
    (for i from 0)
    (when (eql current value)
      (return i))))

(defun clear-queue (queue)
  (setf (left (tail queue)) (head queue)
        (right (head queue)) (tail queue))
  queue)

(defun in-queue-p (value queue)
  (do-queue (val queue)
    (when (eql val value)
      (return T))))

(defun coerce-queue (queue type)
  (ecase type
    (queue
     queue)
    (list
     (iterate (for val in-queue queue)
       (collect val)))
    (vector
     (let ((vec (make-array (size queue))))
       (iterate (for val in-queue queue)
         (for i from 0)
         (setf (aref vec i) val))
       vec))
    (sequence
     (coerce-queue queue 'list))))
