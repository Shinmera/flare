#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defvar *viewer* NIL)
(defvar *progressions* ())

(define-widget main (QMainWindow)
  ())

(define-subwidget (main viewer) (make-instance 'viewer)
  (setf (q+:central-widget main) viewer))

(define-subwidget (main menu) (make-instance 'menu)
  (q+:add-dock-widget main (q+:qt.right-dock-widget-area) menu))

(define-widget menu (QDockWidget)
  ())

(define-initializer (menu setup)
  (setf (q+:features menu) (q+:qdockwidget.dock-widget-movable)))

(define-subwidget (menu container) (q+:make-qwidget)
  (setf (q+:widget menu) container))

(define-subwidget (menu chooser) (q+:make-qcombobox)
  (q+:add-items chooser (mapcar #'string *progressions*)))

;;; EH....
;; Currently this is all tuned for a singular progression
;; at once. Very simplistic, but in the future we might
;; want to extend this to have the ability to run multiple
;; progressions at the same time, like in a real editor.
(define-slot (menu choose) ((name string))
  (declare (connected chooser (activated string)))
  (dolist (progression (progressions (scene *viewer*)))
    (leave progression (scene *viewer*)))
  (enter (progression-instance (find name *progressions* :test #'string-equal))
         (scene *viewer*)))

(define-subwidget (menu reset) (q+:make-qpushbutton "<<"))

(define-slot (menu reset) ()
  (declare (connected reset (clicked)))
  (dolist (progression (progressions (scene *viewer*)))
    (reset progression)))

(define-subwidget (menu start) (q+:make-qpushbutton ">"))

(define-slot (menu start) ()
  (declare (connected start (clicked)))
  (dolist (progression (progressions (scene *viewer*)))
    (start progression)))

(define-subwidget (menu stop) (q+:make-qpushButton "o"))

(define-slot (menu stop) ()
  (declare (connected stop (clicked)))
  (dolist (progression (progressions (scene *viewer*)))
    (stop progression)))

(define-subwidget (menu layout) (q+:make-qvboxlayout container)
  (setf (q+:alignment layout) (q+:qt.align-top))
  (q+:add-widget layout chooser)
  (let ((sub (q+:make-qhboxlayout)))
    (q+:add-widget sub reset)
    (q+:add-widget sub start)
    (q+:add-widget sub stop)
    (q+:add-layout layout sub)))

(defun reset-menu (menu)
  (with-slots-bound (menu menu)
    (loop repeat (q+:size chooser)
          do (q+:remove-item chooser 0))
    (q+:add-items chooser (mapcar #'string *progressions*))
    (let ((progression (first (progressions (scene *viewer*)))))
      (when progression
        (q+:set-current-index
         chooser (position progression *progressions*))))))

(defmacro define-viewer-progression (name &body forms)
  `(progn
     (pushnew ',name *progressions*)
     (when *viewer*
       (reset-menu (slot-value *viewer* 'menu)))
     (define-progression ,name ,@forms)))

(define-widget viewer (QGLWidget)
  ((scene :initform (make-instance 'scene) :accessor scene)))

(define-initializer (viewer setup)
  (setf *viewer* viewer)
  (start scene)
  (when *progressions*
    (enter (progression-instance (first *progressions*))
           scene)))

(define-finalizer (viewer teardown)
  (stop scene)
  (setf *viewer* NIL))

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

(defun main (&key (blocking T))
  (with-main-window (window 'main :name "Flare-Viewer" :blocking blocking)))

(defclass sphere (particle)
  ((size :initarg :size :accessor size)
   (color :initarg :color :accessor color))
  (:default-initargs
   :size 10
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

(defmethod call-with-translation (func target vec)
  (gl:with-pushed-matrix
    (gl:translate (vx vec) (vy vec) (vz vec))
    (funcall func)))

(defmethod paint ((sphere sphere) target)
  (with-translation ((location sphere) target)
    (gl:color (vx (color sphere)) (vy (color sphere)) (vz (color sphere)))
    (draw-sphere (size sphere))))

(define-viewer-progression spinner
  0 0 (T (enter ring :size 100 :children (sphere)))
  0 T (> (increase angle :by 360 :for 1)))
