(defpackage #:flare-staple
  (:nicknames #:org.shirakumo.fraf.flare.staple)
  (:use #:cl))
(in-package #:org.shirakumo.fraf.flare.staple)

(defclass easing (definitions:global-definition) ())

(definitions:define-simple-type-map easing flare:easing)
(definitions:define-simple-object-lookup easing flare:easing)
(definitions:define-simple-documentation-lookup easing flare:easing)
(definitions:define-simple-definition-resolver easing flare:easing)
(defmethod staple:definition-order ((_ easing)) 91)
