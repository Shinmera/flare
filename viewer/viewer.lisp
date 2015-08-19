#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(define-widget main (QMainWindow)
  ())

(define-subwidget (main viewer) (make-instance 'viewer)
  (setf (q+:central-widget main) viewer))

(define-widget viewer (QGLWidget)
  ((scene :initform (make-instance 'flare::scene) :accessor scene)))

(define-initializer (viewer setup)
  (flare::enter (make-instance 'sphere) scene))

(define-subwidget (viewer timer) (q+:make-qtimer viewer)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (viewer update) ()
  (declare (connected timer (timeout)))
  (q+:repaint viewer))

(define-override (viewer paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter viewer)))
    (setf (q+:background painter) (q+:make-qbrush (q+:make-qcolor 0 0 0)))
    (q+:erase-rect painter (q+:rect viewer))

    (q+:translate painter (round (/ (q+:width viewer) 2)) (round (/ (q+:height viewer) 2)))
    
    (flare::update scene)
    (flare::paint scene painter))
  (stop-overriding))

(defun main ()
  (with-main-window (window 'main :name "Flare-Viewer")))

(defclass sphere (flare::particle)
  ((size :initarg :size :accessor size))
  (:default-initargs :size 10))

(defmethod draw-sphere (size &key (lat 8) (lng 8))
  (loop for i from 0 below lat
        for lat0 = (* PI (+ -0.5 (/ (1- i) lat)))
        for lat1 = (* PI (+ -0.5 (/ i lat)))
        for z0 = (sin lat0)
        for zr0 = (cos lat0)
        for z1 = (sin lat1)
        for zr1 = (cos lat1)
        do (gl:with-primitives :quad-strip
             (loop for j from 0 to lng
                   for l = (* 2 PI (/ (1- j) lng))
                   for x = (cos l)
                   for y = (sin l)
                   do (gl:normal (* x zr0 size) (* y zr0 size) z0)
                      (gl:vertex (* x zr0 size) (* y zr0 size) z0)
                      (gl:normal (* x zr1 size) (* y zr1 size) z1)
                      (gl:vertex (* x zr1 size) (* y zr1 size) z1)))))

(defmethod flare::paint ((sphere sphere) target)
  (q+:begin-native-painting target)
  (gl:with-pushed-matrix
    (gl:translate (flare::x sphere) (flare::y sphere) (flare::z sphere))
    (gl:color 255 255 255)
    (draw-sphere (size sphere)))
  (q+:end-native-painting target))