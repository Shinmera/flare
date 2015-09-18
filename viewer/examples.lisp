#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defclass sphere (particle sized-entity)
  ((color :initarg :color :accessor color))
  (:default-initargs
   :color (vec 1 1 1)))

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
  (with-translation ((location sphere) :gl)
    (gl:color (vx (color sphere)) (vy (color sphere)) (vz (color sphere)))
    (draw-sphere (size sphere))))

(define-viewer-progression spinner
  0 0 (T (enter ring :size 100 :children (sphere)))
  0 T (> (increase angle :by 360 :for 1)))

(define-viewer-progression spiral
  0 0 (T (enter ring :size 0 :children (sphere)))
  0 T (> (increase angle :by 360 :for 1)
         (increase size :by 20 :for 1)))
