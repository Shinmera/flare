#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.viewer)
(in-readtable :qtools)

(defvar *main* NIL)

(define-widget main (QMainWindow)
  ())

(define-initializer (main setup)
  (setf *main* main))

(define-finalizer (main teardown)
  (setf *main* NIL))

(define-subwidget (main viewer) (make-instance 'viewer)
  (setf (q+:central-widget main) viewer))

(define-subwidget (main menu) (make-instance 'menu)
  (q+:add-dock-widget main (q+:qt.bottom-dock-widget-area) menu))

(defun scene ()
  (slot-value (slot-value *main* 'viewer) 'scene))

(defun main (&key (blocking T))
  (with-main-window (window 'main :name "Flare-Viewer" :blocking blocking)))
