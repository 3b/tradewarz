(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (world-map :accessor world-map)
   (entities :accessor entities
             :initform (make-hash-table :test 'equal))))

(defun make-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name)))

(defun current-scene ()
  (scene *game*))

(defmethod initialize-instance :after ((object scene) &key)
  (let ((data (read-data "scenes" (name object))))
    (loop for asset in (getf data :assets)
          for width = (getf data :width)
          for height = (getf data :height)
          for tile-size = (getf data :tile-size)
          for tiles = (getf data :tiles) do
          (load-entities object asset)
          (load-map object `(:width ,width
                             :height ,height
                             :tile-size ,tile-size
                             :tiles ,tiles)))))

(defun load-entities (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for entity = (apply #'make-instance 'entity data) do
        (setf (gethash name (entities scene)) entity)))
