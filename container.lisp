(in-package #:org.shirakumo.flare)

(defgeneric name (unit))
(defgeneric enter (unit scene-graph))
(defgeneric leave (unit scene-graph))
(defgeneric clear (container))
(defgeneric objects (container))
(defgeneric name-map (scene-graph))
(defgeneric units (scene-graph))
(defgeneric unit (name scene-graph))
(defgeneric register (unit scene-graph))
(defgeneric deregister (unit scene-graph))
(defgeneric scene-graph (container-unit))

(defclass unit ()
  ((name :initform NIL :initarg :name :reader name)))

(defmethod initialize-instance :before ((unit unit) &key name)
  (check-type name symbol))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type T :identity T)
    (format stream "~@[~a~]" (name unit))))

(defclass container ()
  ((objects :initform (make-indexed-set) :accessor objects)))

(defmethod paint ((container container) target)
  (for:for ((item over container))
    (paint item target)))

(defmethod update ((container container) dt)
  (for:for ((item over container))
    (update item dt)))

(defmethod enter (thing (container container))
  (set-add thing (objects container))
  thing)

(defmethod leave (thing (container container))
  (set-remove thing (objects container))
  thing)

(defmethod clear ((container container))
  (for:for ((item over container))
    (leave item container))
  container)

(defmethod unit (n (container container))
  (set-value-at n (objects container)))

(defmethod (setf unit) (value n (container container))
  (setf (set-value-at n (objects container)) value))

(defmethod for:make-iterator ((container container) &rest args)
  (apply #'for:make-iterator (objects container) args))

(defun map-container-tree (function container)
  (for:for ((item over container))
    (funcall function item)
    (when (typep item 'container)
      (map-container-tree function item))))

(defmacro do-container-tree ((item container &optional return) &body body)
  `(block NIL
     (map-container-tree (lambda (,item) ,@body) ,container)
          ,return))

(defun print-container-tree (container &optional (depth 0))
  (format T "~&~v@{ ~}+ ~a~%" depth container)
  (for:for ((item over container))
    (if (typep item 'container)
        (print-container-tree item (+ depth 2))
        (format T "~&~v@{ ~}| ~a~%" (+ depth 2) item))))

(defmethod describe-object ((container container) stream)
  (format stream "~a
  [~a]

Tree:"
          container (type-of container))
  (print-container-tree container stream)
  (format stream "~&"))

(defclass scene-graph (container)
  ((name-map :initform (make-hash-table :test 'eq) :accessor name-map)))

(defmethod print-object ((scene-graph scene-graph) stream)
  (print-unreadable-object (scene-graph stream :type T :identity T)
    (format stream "~a units" (hash-table-count (name-map scene-graph)))))

(defmethod register :around ((unit unit) (scene-graph scene-graph))
  (when (or (null (name unit))
            (not (eql unit (gethash (name unit) (name-map scene-graph)))))
    (call-next-method)))

(defmethod register ((unit unit) (scene-graph scene-graph))
  (when (name unit)
    (setf (gethash (name unit) (name-map scene-graph)) unit)))

(defmethod deregister ((unit unit) (scene-graph scene-graph))
  (when (eql unit (gethash (name unit) (name-map scene-graph)))
    (remhash (name unit) (name-map scene-graph)))
  unit)

(defmethod enter :after ((unit unit) (scene-graph scene-graph))
  (register unit scene-graph))

(defmethod leave :after ((unit unit) (scene-graph scene-graph))
  (deregister unit scene-graph))

(defmethod units ((scene-graph scene-graph))
  (let ((units ()))
    (do-container-tree (item scene-graph units)
      (push item units))))

(defmethod unit ((name symbol) (scene-graph scene-graph))
  (gethash name (name-map scene-graph)))

(defclass container-unit (container unit)
  ((scene-graph :initform NIL :accessor scene-graph)))

(defmethod print-object ((unit container-unit) stream)
  (print-unreadable-object (unit stream :type T)
    (format stream "~a => ~a" (name unit) (scene-graph unit))))

(defmethod initialize-instance :after ((unit container-unit) &key scene-graph)
  (setf (scene-graph unit) scene-graph))

(defmethod (setf scene-graph) :before (scene-graph (unit container-unit))
  (let ((scene-graph (scene-graph unit)))
    (when scene-graph
      (do-container-tree (item unit)
        (deregister item scene-graph)))))

(defmethod (setf scene-graph) :after ((scene-graph scene-graph) (unit container-unit))
  (when scene-graph
    (do-container-tree (item unit)
      (register item scene-graph))))

(defmethod enter :before ((unit container-unit) (scene-graph scene-graph))
  (when (scene-graph unit)
    (error "~a is already contained in ~a, cannot enter it into ~a."
           unit (scene-graph unit) scene-graph)))

(defmethod leave :before ((unit container-unit) (scene-graph scene-graph))
  (unless (eql (scene-graph unit) scene-graph)
    (error "~a is contained in ~a, cannot leave it from ~a."
           unit (scene-graph unit) scene-graph)))

(defmethod enter :after ((unit unit) (container container-unit))
  (when (scene-graph container)
    (register unit (scene-graph container))))

(defmethod leave :after ((unit unit) (container container-unit))
  (when (scene-graph container)
    (deregister unit (scene-graph container))))

(defmethod register :after ((unit container-unit) (scene-graph scene-graph))
  (setf (scene-graph unit) scene-graph))

(defmethod deregister :after ((unit container-unit) (scene-graph scene-graph))
  (setf (scene-graph unit) NIL))
