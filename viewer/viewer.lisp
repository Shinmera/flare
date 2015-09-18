#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(define-widget viewer (QGLWidget)
  ((scene :initform (make-instance 'scene))))

(define-initializer (viewer setup)
  (start scene)
  (when *progressions*
    (enter (progression-instance (first *progressions*))
           scene)))

(define-finalizer (viewer teardown)
  (stop scene))

(define-subwidget (viewer timer) (q+:make-qtimer viewer)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (viewer update) ()
  (declare (connected timer (timeout)))
  (update scene)
  (q+:repaint viewer))

(define-override (viewer paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter viewer)))
    (setf (q+:background painter) (q+:make-qbrush (q+:make-qcolor 0 0 0)))
    (q+:erase-rect painter (q+:rect viewer))

    (q+:translate painter (round (/ (q+:width viewer) 2)) (round (/ (q+:height viewer) 2)))
    
    (q+:begin-native-painting painter)
    (paint scene painter)
    (q+:end-native-painting painter))
  (stop-overriding))

(defmethod call-with-translation (func target vec)
  (gl:with-pushed-matrix
    (gl:translate (vx vec) (vy vec) (vz vec))
    (funcall func)))
