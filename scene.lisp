(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (root :reader root
         :initarg :root
         :initform :world)
   (world-map :accessor world-map)
   (models :reader models
             :initform (make-hash-table))
   (layer-order :reader layer-order
                :initform '(:map :mob))
   (layers :reader layers
           :initform (make-hash-table))))

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets)
        do (load-models object asset)
           (apply #'load-map object world)))

(defmethod print-object ((object scene) stream)
  (let ((entity-count 0))
    (loop for layer being the hash-value of (layers object)
          do (incf entity-count (length layer)))
    (format stream "Scene: ~:(~a~) (entities: ~a)"
            (name object)
            entity-count)))

(defun make-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (make-layers)
  (generate-map))

(defun current-scene ()
  (scene *game*))

(defun make-layers ()
  (loop for layer-name in (layer-order (current-scene))
        for layers = (layers (current-scene))
        do (setf (gethash layer-name layers)
                 (make-array 10 :fill-pointer 0 :adjustable t))))

(defun get-layer (layer)
  (gethash layer (layers (current-scene))))

(defun get-entities (layer)
  (loop for entity being the elements of (get-layer layer)
        collect entity))

(defun print-models ()
  (loop for model being the hash-value of (models (current-scene))
        do (print model)))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)))
