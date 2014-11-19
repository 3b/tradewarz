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
             :initform '(0 0 0 0))))

(defun get-entity (id &key layer-name)
  (let ((layer (get-layer layer-name)))
    (aref layer id)))

(defun make-entity (model &key layer-name)
  (let* ((layer (get-layer layer-name))
         (entity (make-instance 'entity :id (length layer) :model model)))
    (vector-push-extend entity layer)
    entity))

(defmethod move ((entity entity) x y z)
  (let ((offset (mapcar #'+ (offset entity) `(,x ,y ,z))))
    (setf (offset entity) offset)))

(defmethod rotate ((entity entity) angle x y z)
  (let ((rotation (mapcar #'+ (rotation entity) `(,angle ,x ,y ,z))))
    (setf (rotation entity) rotation)))

(defun update-entities ()
  (loop for layer-name in (layer-order (current-scene))
        for layer = (get-layer layer-name) do
        (loop for entity being the elements of layer do
              (gl:with-pushed-matrix
                (apply #'gl:translate (offset entity))
                (apply #'gl:rotate (rotation entity))
                (draw-model (model entity))))))
