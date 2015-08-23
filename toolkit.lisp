#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

(defmacro define-self-returning-method (name arglist)
  (let ((o (first arglist)))
    `(defmethod ,name :around ,arglist
       (declare (ignore ,@(lambda-fiddle:extract-lambda-vars (cdr arglist))))
       (call-next-method)
       ,(if (listp o) (first o) o))))
