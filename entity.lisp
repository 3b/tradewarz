(in-package :tradewarz)

(defclass entity ()
  ((id :reader id
       :initarg :id)
   (model :reader model
          :initarg :model)
   (x :accessor x
      :initarg :x
      :initform 0)
   (y :accessor y
      :initarg :y
      :initform 0)
   (z :accessor z
      :initarg :z
      :initform 0)))

(defun get-entity (id)
  (gethash id (entities (current-scene))))

(defun make-entity (model)
  (let* ((entities (entities (current-scene)))
         (all-ids (cons 0 (hash-table-keys entities)))
         (id (1+ (apply #'max all-ids)))
         (entity (make-instance 'entity :id id :model model)))
    (setf (gethash id entities) entity)))

(defmethod move ((entity entity) x y z)
  (setf (x entity) (+ x (x entity)))
  (setf (y entity) (+ y (y entity)))
  (setf (z entity) (+ z (z entity))))

(defun update-entities ()
  (loop for entity in (nreverse (hash-table-values (entities (current-scene))))
        for pos = `(,(x entity) ,(y entity) ,(z entity)) do
        (gl:with-pushed-matrix
          (apply #'gl:translate pos)
          (draw-model (model entity)))))
