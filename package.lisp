#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:flare-queue
  (:nicknames #:org.shirakumo.flare.queue)
  (:use #:cl #:for)
  (:export
   #:queue
   #:make-queue
   #:on-queue
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
   #:map-set
   #:on-set
   #:in-set
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
  (:use #:cl #:3d-vectors #:flare-queue #:flare-indexed-set)
  (:shadow #:leave)
  ;; animation.lisp
  (:export
   #:animatable
   #:progression-definition
   #:progression
   #:animation
   #:change
   
   #:progressions
   #:add-progression
   #:remove-progression
   #:progression-instance
   #:animations
   #:instances
   #:present-animations
   #:past-animations
   #:future-animations
   #:animations
   #:beginning
   #:duration
   #:changes
   #:selector
   #:tick)
  ;; clock.lisp
  (:export
   #:update
   #:stop
   #:start
   #:reset
   #:running
   #:timescale
   #:synchronize
   #:clock

   #:clock)
  ;; change.lisp
  (:export
   #:define-change-parser
   #:change
   #:print-change
   #:print
   #:call-change
   #:call
   #:operation
   #:enter-operation
   #:objects
   #:creator
   #:enter
   #:create
   #:leave-operation
   #:leave
   #:objects
   #:tween
   #:slot-tween
   #:slot
   #:originals
   #:original-value
   #:range-tween
   #:range-slot-tween
   #:set
   #:constant-tween
   #:increase-slot-tween
   #:increase
   #:call-slot-tween
   #:call-accessor-tween
   #:calc)
  ;; container.lisp
  (:export
   #:objects
   #:insert
   #:withdraw
   #:name-map
   #:units
   #:unit
   #:clear
   #:enter
   #:leave
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
   #:ease-object
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
  ;; forms.lisp
  (:export
   #:oriented-entity
   #:sized-entity
   #:formation
   #:particle
   #:arc
   #:orientation
   #:size
   #:up
   #:angle
   #:spacing
   #:ring)
  ;; parser.lisp
  (:export
   #:make-progression
   #:progression-definition
   #:remove-progression-definition
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
   #:scene
   #:location
   #:scene-unit
   #:entity))
