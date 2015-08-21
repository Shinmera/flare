#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defvar *easings* (make-hash-table :test 'eql))

(defun easing (name)
  (gethash name *easings*))

(defun (setf easing) (func name)
  (setf (gethash name *easings*) func))

(defun remove-easing (name)
  (remhash name *easings*))

(defmacro define-easing (name (x) &body body)
  `(setf (easing ',name)
         (lambda (,x)
           ,@body)))

(defun ease (x by &optional (from 0) (to 1))
  (let ((easing (or (easing by)
                    (error "No such easing ~s found." by))))
    (+ from (* (funcall easing x) (- to from)))))

(defun ease-vec (x by &optional (from (vec 0 0 0)) (to (normalize (vec 1 1 1))))
  (vec (ease x by (x from) (x to))
       (ease x by (y from) (y to))
       (ease x by (z from) (z to))))

(define-easing linear (x)
  x)

(define-easing quad-in (x)
  (expt x 2))

(define-easing quad-out (x)
  (- (* x (- x 2))))

(define-easing quad-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 2) 2)
        (- (/ (1- (* (decf x) (- x 2))) 2)))))

(define-easing cubic-in (x)
  (expt x 3))

(define-easing cubic-out (x)
  (1+ (expt (1- x) 3)))

(define-easing cubic-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 3) 2)
        (/ (+ (expt (- x 2) 3) 2) 2))))

(define-easing quart-in (x)
  (expt x 4))

(define-easing quart-out (x)
  (- (1- (expt (1- x) 4))))

(define-easing quart-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 4) 2)
        (- (/ (- (expt (- x 2) 4) 2) 2)))))

(define-easing quint-in (x)
  (expt x 5))

(define-easing quint-out (x)
  (1+ (expt (1- x) 5)))

(define-easing quint-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 5) 2)
        (/ (+ (expt (- x 2) 5) 2) 2))))

(define-easing sine-in (x)
  (1+ (- (cos (* x (/ PI 2))))))

(define-easing sine-out (x)
  (sin (* x (/ PI 2))))

(define-easing sine-in-out (x)
  (- (/ (1- (cos (* PI x))) 2)))

(define-easing expo-in (x)
  (if (= 0 x)
      0
      (expt 2 (* 10 (1- x)))))

(define-easing expo-out (x)
  (if (= 1 x)
      1
      (1+ (- (expt 2 (* x -10))))))

(define-easing expo-in-out (x)
  (case x
    ((1 0) x)
    (T (let ((x (* x 2)))
         (if (< x 1)
             (/ (expt 2 (* 10 (1- x))) 2)
             (/ (+ (- (expt 2 (* x -10))) 2) 2))))))

(define-easing circ-in (x)
  (- (1- (sqrt (- 1 (expt x 2))))))

(define-easing circ-out (x)
  (sqrt (- 1 (expt (1- x) 2))))

(define-easing circ-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (- (/ (1- (sqrt (- 1 (expt x 2)))) 2))
        (/ (1+ (sqrt (- 1 (expt x 2)))) 2))))

(define-easing back-in (x)
  (let ((s 1.70158))
    (* (expt x 2) (- (* (1+ s) x) s))))

(define-easing back-out (x)
  (let ((s 1.70158))
    (1+ (* (expt x 2) (+ (* (1+ s) x) s)))))

(define-easing back-in-out (x)
  (let ((s (* 1.70158 1.525))
        (x (* x 2)))
    (if (< x 1)
        (/ (* (expt x 2) (- (* (1+ s) x) s)) 2)
        (/ (+ (* (expt x 2) (+ (* (1+ s) x) s)) 2) 2))))

(define-easing elastic-in (x)
  (case x
    ((0 1) x)
    (T (let* ((p 0.3)
              (s (/ p 4)))
         (- (* (expt 2 (* x 10)) (sin (/ (* (- x s) 2 PI) p))))))))

(define-easing elastic-out (x)
  (case x
    ((0 1) x)
    (T (let* ((p 0.3)
              (s (/ p 4)))
         (1+ (* (expt 2 (* x -10)) (sin (/ (* (- x s) 2 PI) p))))))))

(define-easing elastic-in-out (x)
  (case x
    ((0 1) x)
    (T (let* ((x (* x 2))
              (p (* 0.3 1.5))
              (s (/ p 4)))
         (if (< x 1)
             (- (/ (* (expt 2 (* (1- x) 10)) (sin (/ (* (- (1- x) s) 2 PI) p))) 2))
             (1+ (/ (* (expt 2 (* x -10)) (sin (/ (* (- (1- x) s) 2 PI) p))) 2)))))))

(define-easing bounce-in (x)
  (- 1 (ease (- 1 x) 'bounce-out)))

(define-easing bounce-out (x)
  (let ((s 7.5625)
        (p 2.75))
    (cond ((< x (/ 1 p))
           (* s (expt x 2)))
          ((< x (/ 2 p))
           (+ (* s (expt (- x (/ 1.5 p)) 2)) 0.75))
          ((< x (/ 2.5 p))
           (+ (* s (expt (- x (/ 2.25 p)) 2)) 0.9375))
          (T
           (+ (* s (expt (- x (/ 2.625 p)) 2)) 0.984375)))))

(define-easing bounce-in-out (x)
  (if (< x 0.5)
      (/ (ease (* x 2) 'bounce-in) 2)
      (/ (1+ (ease (1- (* x 2)) 'bounce-out)) 2)))
