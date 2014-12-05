(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (root :reader root
         :initarg :root
         :initform (make-instance 'scene-node))
   (layers :accessor layers
           :initform (make-hash-table))
   (world-map :accessor world-map)
   (models :reader models
           :initform (make-hash-table))))

(defclass scene-node ()
  ((model :reader model
          :initarg :model
          :initform nil)
   (parent :accessor parent
           :initarg :parent
           :initform nil)
   (children :accessor children
             :initarg :children
             :initform (make-hash-table :test 'eq))
   (local-basis :accessor local-basis
                :initarg :local-basis
                :initform (matrix-identity-new))
   (world-basis :accessor world-basis
                :initarg :world-basis
                :initform (matrix-identity-new))
   (dv :accessor dv
       :initarg :dv
       :initform (make-vector))
   (dr :accessor dr
       :initarg :dr
       :initform (make-vector))
   (drv :accessor drv
        :initarg :drv
        :initform (make-vector))
   (dtv :accessor dtv
        :initarg :dtv
        :initform (make-vector))
   (rotation :accessor rotation
             :initarg :rotation
             :initform (make-vector))
   (movement :accessor translation
             :initarg :translation
             :initform (make-vector))
   (rotatingp :accessor rotatingp
              :initarg :rotatingp
              :initform nil)
   (movingp :accessor movingp
            :initarg :movingp
            :initform nil)))

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets)
        do (load-models object asset)
           (apply #'load-map object world)))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do
        (setf (gethash name (models scene)) model)
        (when (object model)
          (setf (lines model) (load-obj (object model))))))

(defun current-scene ()
  (scene *game*))

(defun load-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (generate-map)

  (defparameter *e1* (make-node :tank))

  ;; test entities
  (add-node *e1*)
  (setf (movingp *e1*) t)
  (vector-modify (dr *e1*) (/ pi -2) (/ pi 2) pi)
  (vector-modify (dv *e1*) 0 0 -8))

(defun make-node (model)
  (let ((node (make-instance 'scene-node :model model)))
    node))

(defmethod add-child ((parent scene-node) (child scene-node))
  (setf (gethash child (children parent)) child)
  (setf (parent child) parent))

(defun add-node (node &key parent)
  (add-child (or parent (root (current-scene))) node)) 

(defun loop-scene (func &optional parent level)
  (let ((parent (or parent (root (current-scene))))
        (level (or level 0)))
    (funcall func parent level)
    (loop with level = (incf level)
          for child being the hash-values of (children parent)
          do (loop-scene func child level))))

(defun update-scene ()
  (loop-scene #'update-node)
  (sort-layers))

(defun update-node (node &optional level)
  (declare (ignore level))
  (update-local-basis node)
  (update-world-basis node))

(defun update-local-basis (node)
  (matrix-translate (dv node) (local-basis node))
  (vector-clear (dv node))
  (matrix-rotate (dr node) (local-basis node))
  (vector-clear (dr node))
  (when (movingp node)
    (matrix-translate (dtv node) (local-basis node)))
  (when (rotatingp node)
    (matrix-rotate (drv node) (local-basis node))))

(defun update-world-basis (node)
  (if (and (parent node) (model node))
    (matrix-multiply
      (world-basis (parent node))
      (local-basis node)
      (world-basis node))))

(defun node-depth (node &optional level)
  (let ((depth (vy (matrix-get-translation-new (world-basis node)))))
    (push (list node depth) (gethash level (layers (current-scene))))))

(defun sort-layers ()
  (loop-scene #'node-depth)
  (loop with layers = (layers (current-scene))
        for layer being the hash-keys of layers
        for unsorted = (copy-seq (gethash layer layers))
        do (setf (gethash layer layers) (sort unsorted #'< :key #'cadr))))

(defun render-scene ()
  (loop with layers = (layers (current-scene))
        for layer in (hash-table-keys layers)
        do (loop for node-data in (gethash layer layers)
                 do (render-node (car node-data)))
           (remhash layer layers)))

(defun render-node (node)
  (let ((model (get-model (model node))))
    (when model
      (gl:bind-texture :texture-2d (texture-id model))
      (gl:with-primitive (primitive model)
        (loop with vertex-vector = (make-vector)
              with normal-vector = (make-vector)
              with size = (apply #'make-vector (get-size model))
              for (normal vertex texture color) in (lines model)
              do (apply #'gl:color color)
                 (apply #'gl:tex-coord texture)
                 (apply #'vector-modify vertex-vector vertex)
                 (vector-multiply vertex-vector size vertex-vector)
                 (matrix-apply (world-basis node) vertex-vector vertex-vector)
                 (when normal
                   (apply #'vector-modify normal-vector normal)
                   (matrix-apply (world-basis node) normal-vector normal-vector)
                   (apply #'gl:normal (vector->list normal-vector)))
                 (apply #'gl:vertex (vector->list vertex-vector)))))))
