(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (world-map :accessor world-map)
   (entities :reader entities
             :initform (make-hash-table :test 'equal))))

(defun make-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name)))

(defun current-scene ()
  (scene *game*))

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets) do
        (load-entities object asset)
        (apply #'load-map object world)))

(defun load-entities (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for entity = (apply #'make-instance 'entity data) do
        (setf (gethash name (entities scene)) entity)))
