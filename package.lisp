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
   #:vec=
   #:copy
   #:x
   #:y
   #:z
   #:size
   #:translate
   #:-translate
   #:scale
   #:rotate-around
   #:rotate
   #:normalize
   #:translated
   #:-translated
   #:scaled 
   #:rotated
   #:normalized
   #:cross
   #:dot))

(defpackage #:flare-queue
  (:nicknames #:org.shirakumo.flare.queue)
  (:use #:cl)
  (:export
   #:queue
   #:make-queue
   #:loop-queue
   #:map-queue
   #:do-queue
   #:enqueue
   #:dequeue
   #:queue-remove
   #:queue-size
   #:queue-first
   #:queue-last
   #:queue-index-of
   #:queue-value-at
   #:clear-queue
   #:in-queue-p
   #:coerce-queue))

(defpackage #:flare-indexed-set
  (:nicknames #:org.shirakumo.flare.indexed-set)
  (:use #:cl #:org.shirakumo.flare.queue)
  (:shadow #:set)
  (:export
   #:indexed-set
   #:make-indexed-set
   #:loop-set
   #:map-set
   #:do-set
   #:set-add
   #:set-remove
   #:set-size
   #:set-first
   #:set-last
   #:set-value-at
   #:set-index-of
   #:in-set-p
   #:clear-set
   #:coerce-set))

(defpackage #:flare
  (:nicknames #:org.shirakumo.flare)
  (:use #:cl #:flare-vector #:flare-queue #:flare-indexed-set)
  ;; animation.lisp
  (:export
   #:tick
   #:add-animation
   #:remove-animation
   #:perform
   #:initial-value
   #:animations
   #:animation
   #:start
   #:duration
   #:selector
   #:changes

   #:progression
   #:animation)
  ;; clock.lisp
  (:export
   #:update
   #:stop
   #:start
   #:reset
   #:running
   #:synchronize
   #:clock

   #:clock)
  ;; change.lisp
  (:export
   #:field
   #:from
   #:to
   #:from
   #:by
   #:ease-func

   #:change
   #:tween
   #:scale
   #:rotate
   #:set!
   #:increase
   #:edit
   #:enter
   #:leave
   #:delegating-change
   #:every.)
  ;; container.lisp
  (:export
   #:objects
   #:insert
   #:withdraw
   #:name-map
   #:units
   #:unit
   #:enter
   #:leave
   #:object
   #:name
   #:collective
   #:map-container-tree
   #:do-container-tree
   #:print-container-tree

   #:container
   #:collective
   #:unit)
  ;; easings.lisp
  (:export
   #:easing
   #:remove-easing
   #:define-easing
   #:ease
   #:ease-vec
   #:linear
   #:quad-in
   #:quad-out
   #:quad-in-out
   #:cubic-in
   #:cubic-out
   #:cubic-in-out
   #:quart-in
   #:quart-out
   #:quart-in-out
   #:quint-in
   #:quint-out
   #:quint-in-out
   #:sine-in
   #:sine-out
   #:sine-in-out
   #:expo-in
   #:expo-out
   #:expo-in-out
   #:circ-in
   #:circ-out
   #:circ-in-out
   #:back-in
   #:back-out
   #:back-in-out
   #:elastic-in
   #:elastic-out
   #:elastic-in-out
   #:bounce-in
   #:bounce-out
   #:bounce-in-out)
  ;; parser.lisp
  (:export
   #:define-progression)
  ;; paintable.lisp
  (:export
   #:call-with-translation
   #:visibility
   #:paint
   #:with-translation
   
   #:target
   #:paintable)
  ;; scene.lisp
  (:export
   #:progressions
   #:add-progression
   #:remove-progression
   #:scene
   #:location
   #:orientation
   #:size
   #:up
   #:angle

   #:scene
   #:scene-unit
   #:entity
   #:oriented-entity
   #:sized-entity
   #:formation
   #:particle
   #:ring))
