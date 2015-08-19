#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:flare-vector
  (:nicknames #:org.shirakumo.flare.vector)
  (:use #:cl)
  (:export
   #:vec
   #:copy
   #:x
   #:y
   #:z
   #:size
   #:translate
   #:scale
   #:rotate-around
   #:rotate
   #:normalize
   #:translated
   #:scaled 
   #:rotated
   #:normalized
   #:cross
   #:dot))

(defpackage #:flare-queue
  (:nicknames #:org.shirakumo.flare.queue)
  (:us #:cl)
  (:export
   #:queue
   #:make-queue
   #:enqueue
   #:dequeue
   #:queue-size
   #:in-queue-p
   #:queue-first
   #:queue-last
   #:queue-index-of
   #:queue-value-at
   #:loop-queue
   #:map-queue
   #:do-queue
   #:coerce-queue))

(defpackage #:flare-indexed-set
  (:nicknames #:org.shirakumo.flare.indexed-set)
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:indexed-set
   #:make-indexed-set
   #:set-add
   #:set-remove
   #:in-set-p
   #:set-size
   #:set-clear
   #:set-first
   #:set-last
   #:set-value-at
   #:set-index-of
   #:loop-set
   #:map-set
   #:do-set
   #:coerce-set))

(defpackage #:flare
  (:nicknames #:org.shirakumo.flare)
  (:use #:cl #:flare-vector #:flare-indexed-set))
