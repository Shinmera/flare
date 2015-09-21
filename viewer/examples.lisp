#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defclass colored-entity (entity)
  ((color :initarg :color :accessor color))
  (:default-initargs
   :color (vec 1 1 1)))

(defmethod initialize-instance :after ((entity colored-entity) &key)
  (setf (color entity) (color entity)))

(defmethod (setf color) ((vec vec) (entity colored-entity))
  (setf (slot-value entity 'color)
        (q+:make-qcolor
         (round (* 255 (vx vec)))
         (round (* 255 (vy vec)))
         (round (* 255 (vz vec))))))

(defmethod (setf color) ((obj qobject) (entity colored-entity))
  (cond ((qtypep obj "QColor")
         (call-next-method))
        (T
         (error "Don't know how to use ~a as a color." obj))))

(defclass sphere (particle sized-entity colored-entity)
  ())

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

(defmethod paint ((sphere sphere) target)
  (q+:begin-native-painting target)
  (with-translation ((location sphere) :gl)
    (gl:color (q+:red (color sphere))
              (q+:green (color sphere))
              (q+:blue (color sphere)))
    (draw-sphere (size sphere)))
  (q+:end-native-painting target))

(defclass spark (particle sized-entity colored-entity)
  ()
  (:default-initargs
   :size 50))

(defmethod paint ((spark spark) target)
  (let ((size (round (size spark))))
    (with-translation ((location spark) target)
      (with-finalizing ((gradient (q+:make-qradialgradient (/ size 2) (/ size 2) (/ size 2))))
        (setf (q+:color-at gradient 0) (color spark))
        (setf (q+:color-at gradient 1) (q+:make-qcolor 0 0 0 0))
        (setf (q+:brush target) (q+:make-qbrush gradient))
        (setf (q+:pen target) (q+:qt.no-pen))
        (q+:draw-ellipse target 0 0 size size)))))

(define-viewer-progression spinner
  0 0 (T (enter ring :size 100 :children (sphere :size 10)))
  0 T (> (increase angle :by 360 :for 1)))

(define-viewer-progression spiral
  0 0 (T (enter ring :size 0 :children (sphere :size 10)))
  0 T (> (increase angle :by 360 :for 1)
         (increase size :by 10 :for 1)))

(define-viewer-progression pendulum
  0 0 (T (enter ring :size 100 :children (sphere :size 10)))
  0 T (> (calc angle :to (+ 90 (* (sin (* clock 4)) 40)))))
