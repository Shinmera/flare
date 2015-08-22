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
   #:compile-change
   #:perform
   #:initial-value
   #:animations
   #:start
   #:end
   #:selector
   #:changes
   #:field
   #:from
   #:to
   #:from
   #:by
   #:ease-func
   #:progression
   #:animation
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
  ;; scene.lisp
  (:export
   #:call-with-translation
   #:with-translation
   #:update
   #:stop
   #:running
   #:start
   #:reset
   #:clock
   #:visibility
   #:paint
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
   #:progressions
   #:add-progression
   #:remove-progression
   #:scene

   #:target
   #:timer
   #:paintable
   #:container
   #:map-container-tree
   #:do-container-tree
   #:print-container-tree
   #:collective
   #:unit
   #:scene
   #:scene-unit
   #:entity
   #:oriented-entity
   #:sized-entity
   #:formation
   #:particle
   #:ring
   #:location
   #:orientation
   #:size
   #:up
   #:angle))
