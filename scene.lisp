(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (tile-map :reader tile-map
             :initform nil) 
   (entities :accessor entities
             :initform (make-hash-table :test 'equal))))

(defun make-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name)))

(defmethod initialize-instance :after ((object scene) &key)
  (let ((data (read-data "scenes" (name object))))
    (loop for asset in (getf data :assets)
          for tile in (getf data :map) do
          (load-entities object asset))))

(defun load-entities (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for entity = (apply #'make-instance 'entity data) do
        (setf (gethash name (entities scene)) entity)))
