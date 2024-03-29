(in-package #:org.shirakumo.flare.indexed-set)

(defclass indexed-set (queue)
  ((set :initform (make-hash-table :test 'eql) :accessor set)))

(defun make-indexed-set ()
  (make-instance 'indexed-set))

(setf (fdefinition 'map-set) (fdefinition 'map-queue))
(setf (macro-function 'do-set) (macro-function 'do-queue))

(defun set-add (value set)
  (cond ((gethash value (set set))
         (values set NIL))
        (T
         (let ((cell (flare-queue::make-cell value NIL NIL)))
           (setf (gethash value (set set)) cell)
           (flare-queue::cell-insert-before cell (flare-queue::tail set)))
         (flare-queue::set-size (1+ (flare-queue::size set)) set)
         (values set T))))

(defun set-add-before (after value set)
  (cond ((gethash value (set set))
         (values set NIL))
        (T
         (let ((pivot (gethash after (set set)))
               (cell (flare-queue::make-cell value NIL NIL)))
           (setf (gethash value (set set)) cell)
           (flare-queue::cell-insert-before cell pivot))
         (flare-queue::set-size (1+ (flare-queue::size set)) set)
         (values set T))))

(defun set-add-after (before value set)
  (cond ((gethash value (set set))
         (values set NIL))
        (T
         (let ((pivot (gethash before (set set)))
               (cell (flare-queue::make-cell value NIL NIL)))
           (setf (gethash value (set set)) cell)
           (flare-queue::cell-insert-after cell pivot))
         (flare-queue::set-size (1+ (flare-queue::size set)) set)
         (values set T))))

(defun set-remove (value set)
  (let ((cell (gethash value (set set))))
    (cond (cell
           (remhash value (set set))
           (flare-queue::cell-remove cell)
           (flare-queue::set-size (1- (flare-queue::size set)) set)
           (values set T))
          (T
           (values set NIL)))))

(setf (fdefinition 'set-size) (fdefinition 'queue-size))
(setf (fdefinition 'set-first) (fdefinition 'queue-first))
(setf (fdefinition 'set-last) (fdefinition 'queue-last))
(setf (fdefinition 'set-value-at) (fdefinition 'queue-value-at))
(setf (fdefinition '(setf set-value-at)) (fdefinition '(setf queue-value-at)))
(setf (fdefinition 'set-index-of) (fdefinition 'queue-index-of))
(setf (fdefinition 'clear-set) (fdefinition 'clear-queue))

(defun in-set-p (value set)
  (nth-value 1 (gethash value (set set))))

(defun coerce-set (set type)
  (case type
    (indexed-set
     set)
    (hash-table
     (let ((table (make-hash-table :test 'eql)))
       (do-queue (val set table)
         (setf (gethash val table) val))))
    (T
     (coerce-queue set type))))
