#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defgeneric location (entity))

(defclass scene (scene-graph clock paintable animatable)
  ())

(defclass entity (container-unit paintable animatable)
  ((location :initarg :location :accessor location))
  (:default-initargs
   :location (vec 0 0 0)))
