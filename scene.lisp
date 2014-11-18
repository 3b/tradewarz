(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (world-map :accessor world-map)
   (models :reader models
             :initform (make-hash-table))
   (entities :reader entities
             :initform (make-hash-table))))

(defun make-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (generate-map))

(defun current-scene ()
  (scene *game*))

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets) do
        (load-models object asset)
        (apply #'load-map object world)))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model data) do
        (setf (gethash name (models scene)) model)))
