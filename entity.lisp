(in-package :tradewarz)

(defclass entity ()
  ((id :reader id
       :initarg :id)
   (model :reader model
          :initarg :model)
   (offset :accessor offset
           :initarg :offset
           :initform '(0 0 0))
   (rotation :accessor rotation
             :initarg :rotation
             :initform '(0 0 0))))

(defun get-entity (id &key layer)
  (aref (get-layer layer) id))

(defun make-entity (model &key layer)
  (let* ((layer (get-layer layer))
         (entity (make-instance 'entity :id (length layer) :model model)))
    (vector-push-extend entity layer)
    entity))

(defmethod move ((entity entity) offset)
  (let ((offset (mapcar #'+ (offset entity) offset)))
    (setf (offset entity) offset)))

(defmethod rotate ((entity entity) angles)
  (setf (rotation entity) angles))

(defun update-entities ()
  (loop for layer-name in (layer-order (current-scene))
        for layer = (get-layer layer-name) do
        (loop for entity being the elements of layer do
              (gl:with-pushed-matrix
                (apply #'gl:translate (offset entity))
                (gl:rotate (first (rotation entity)) 1 0 0)
                (gl:rotate (second (rotation entity)) 0 1 0)
                (gl:rotate (third (rotation entity)) 0 0 1)
                (draw-model (model entity))))))
