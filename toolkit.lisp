(in-package #:org.shirakumo.flare)

(defmacro define-self-returning-method (name arglist)
  (let ((o (first arglist)))
    `(defmethod ,name :around ,arglist
       (declare (ignore ,@(lambda-fiddle:extract-lambda-vars (cdr arglist))))
       (call-next-method)
       ,(if (listp o) (first o) o))))

(defun ensure-sorted (vec sorting &key key)
  (let ((sorted (stable-sort vec sorting :key key)))
    (unless (eq vec sorted)
      (loop for i from 0 to (length vec)
            do (setf (aref vec i) (aref sorted i))))
    vec))
