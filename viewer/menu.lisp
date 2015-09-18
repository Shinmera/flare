#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defvar *progressions* ())

(define-widget menu (QDockWidget)
  ())

(define-initializer (menu setup)
  (setf (q+:features menu) (q+:qdockwidget.dock-widget-movable))
  (setf (q+:title-bar-widget menu) (q+:make-qwidget)))

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
  (dolist (progression (progressions (scene)))
    (reset progression)
    (leave progression (scene)))
  (enter (progression-instance (find name *progressions* :test #'string-equal))
         (scene)))

(define-subwidget (menu reset) (q+:make-qpushbutton (q+:qicon-from-theme "media-seek-backward") "<<"))

(define-slot (menu reset) ()
  (declare (connected reset (clicked)))
  (dolist (progression (progressions (scene)))
    (reset progression)))

(define-subwidget (menu start) (q+:make-qpushbutton (q+:qicon-from-theme "media-playback-start") ">"))

(define-slot (menu start) ()
  (declare (connected start (clicked)))
  (dolist (progression (progressions (scene)))
    (start progression)))

(define-subwidget (menu stop) (q+:make-qpushButton (q+:qicon-from-theme "media-playback-stop") "o"))

(define-slot (menu stop) ()
  (declare (connected stop (clicked)))
  (dolist (progression (progressions (scene)))
    (stop progression)))

(define-subwidget (menu layout) (q+:make-qvboxlayout container)
  (setf (q+:alignment layout) (q+:qt.align-top))
  (let ((sub (q+:make-qhboxlayout)))
    (setf (q+:alignment sub) (q+:qt.align-left))
    (q+:add-widget sub chooser)
    (q+:add-widget sub reset)
    (q+:add-widget sub start)
    (q+:add-widget sub stop)
    (q+:add-layout layout sub)))

(defun reset-menu (menu)
  (with-slots-bound (menu menu)
    (loop repeat (q+:count chooser)
          do (q+:remove-item chooser 0))
    (q+:add-items chooser (mapcar #'string *progressions*))
    (let ((progression (first (progressions (scene)))))
      (when progression
        (q+:set-current-index
         chooser (position (flare::definition progression) *progressions*
                           :key #'progression-definition))))))

(defmacro define-viewer-progression (name &body forms)
  `(progn
     (pushnew ',name *progressions*)
     (when *main*
       (reset-menu (slot-value *main* 'menu)))
     (define-progression ,name ,@forms)))
