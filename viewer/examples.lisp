(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defvar *opacity* 1.0)

(defmethod paint :around ((entity entity) target)
  (let ((*opacity* (* *opacity* (visibility entity))))
    (setf (q+:opacity target) *opacity*)
    (call-next-method)))

(defclass colored-entity (entity)
  ((color :initarg :color :accessor color))
  (:default-initargs
   :color (vec 1 1 1)))

(defmethod initialize-instance :after ((entity colored-entity) &key)
  (setf (color entity) (color entity)))

(defmethod (setf color) ((vec vec3) (entity colored-entity))
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
   :size 20))

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

(define-viewer-progression show
   0  0 (T (enter ring :name :ground :size 130 :children (spark :n 5))
           (enter ring :name :phase1 :size 120 :children (spark :n 10)))
   0 60 (:ground (increase angle :by 3))
   0  5 (:phase1 (increase angle :by 8))
   0 10 (:phase1 (set size :to 300 :ease quad-in-out))
   5 15 (:phase1 (increase angle :by 5))
  10 15 (:phase1 (set visibility :to 0 :ease linear))
  15 15 (:phase1 (leave))
  16 16 (T (enter ring :name :phase2 :size 120 :children (spark :n 7)))
  16 20 (:phase2 (set size :to 300 :ease quad-in-out))
  16 20 (:phase2 (set angle :to -180 :ease quart-in))
  20 60 (:phase2 (increase angle :by -3))
  20 20 (T (enter ring :name :phase3 :size 120 :children (ring :size 20 :n 5 :children (spark :n 2))))
  20 50 (:phase3 (increase angle :by 2))
  20 50 ((:phase3 >) (increase angle :by 3))
  20 25 (:phase3 (set size :to 250 :ease quad-in-out))
  25 30 ((:phase3 >) (set size :to 100 :ease quad-in-out))
  30 40 (:phase3 (set size :to 400 :ease quad-in-out))
  40 50 (:phase3 (set size :to 120 :ease quad-in-out))
  48 50 (:phase3 (set visibility :to 0))
  50 50 (:phase3 (leave))
  50 60 (:phase2 (set size :to 120 :ease expo-in))
  60 60 ((T >) (leave)))
