(in-package :tradewarz)

(defclass entity ()
  ((id :reader id
       :initarg :id)
   (model :reader model
          :initarg :model)
   (parent :accessor parent
           :initarg :parent
           :initform nil)
   (children :accessor children
             :initarg :children
             :initform nil)
   (local-basis :accessor local-basis
                :initarg :local-basis
                :initform (matrix-identity-new))
   (world-basis :accessor world-basis
                :initarg :world-basis
                :initform (matrix-identity-new))
   (offset :accessor offset
           :initform '(0 0 0))
   (rotation :accessor rotation
             :initarg :rotation
             :initform '(0 0 0 0))))

(defmethod print-object ((object entity) stream)
  (format stream "Entity: ~:(~a~)" (model object)))

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
  (loop for layer-name in (layer-order (current-scene)) do
        (loop for entity in (get-entities layer-name) do
              (gl:with-pushed-matrix
                (apply #'gl:translate (offset entity))
                (apply #'gl:rotate (rotation entity))
                (draw-model (model entity))))))
