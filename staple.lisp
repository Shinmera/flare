(in-package #:cl-user)

(defclass symb-easing (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-easing))
  (flare:easing (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb symb-easing))
  (documentation (staple:symb-symbol symb) 'flare:easing))

(defmethod staple:symb-type-order ((symb (eql 'symb-easing)))
  (1+ (staple:symb-type-order 'symb-function)))

(staple:define-simple-converter symb-easing flare:easing)

(defun staple ()
  (staple:generate :flare
                   :packages '(:flare :flare-queue :flare-indexed-set)
                   :if-exists :supersede))
