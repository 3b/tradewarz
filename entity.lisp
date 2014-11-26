(in-package :tradewarz)

(defclass entity ()
  ((id :reader id
       :initarg :id)
   (model :reader model
          :initarg :model
          :initform nil)))

(defmethod print-object ((object entity) stream)
  (format stream "Entity: ~:(~a~)" (model object)))

(defun get-entity (id &key layer)
  (aref (get-layer layer) id))

(defun make-entity (model &key layer)
  (let* ((layer (get-layer layer))
         (entity (make-instance 'scene-node :id (length layer) :model model)))
    (vector-push-extend entity layer)
    entity))

(defmethod add-child ((parent entity) (child entity))
  (setf (gethash child (children parent)) child)
  (setf (parent child) parent))
