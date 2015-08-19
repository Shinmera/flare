#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defclass animation-queue (queue)
  ())

(defmethod update ((queue animation-queue))
  ())

(defclass animation ()
  (start
   end
   selector
   changes))

(defgeneric apply (change time))

(defclass change ()
  ())

(defclass tween (change)
  (from
   to
   by
   ease))

(defclass edit (change)
  ())
