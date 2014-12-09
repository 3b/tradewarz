(in-package :tradewarz)

(defclass scene (frame)
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
   (dirtyp :accessor dirtyp
           :initarg dirtyp
           :initform t)
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
        do (setf (gethash name (models scene)) model)
           (when (object model)
             (setf (lines model) (load-obj (object model))))))

(defun current-scene ()
  (scene *game*))

(defun load-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (generate-map)

  ;; test entities
  (defparameter *e1* (make-node :tank))
  (let ((axes (make-node :axes)))
    (add-node axes)
    (vector-modify (dv axes) -32 -32 0)
    (add-node *e1*)
    (setf (movingp *e1*) t)
    (setf (rotatingp *e1*) t)
    (vector-modify (drv *e1*) 0 0 0.01)
    (vector-modify (dv *e1*) 0 0 8)))

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

(defun update-scene (&optional dt)
  (loop-scene #'update-node)
  (loop-scene #'render-node))

(defun update-node (node &optional level)
  (declare (ignore level))
  (when (or (dirtyp node)
             (rotatingp node)
             (movingp node))
    (update-local-basis node)
    (update-world-basis node)
    (setf (dirtyp node) nil)))

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

(defun render-node (node &optional level)
  (let ((model (get-model (model node))))
    (when model
      (gl:with-pushed-matrix
        (gl:mult-matrix (convert-to-opengl-new (world-basis node)))
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
                   (when normal
                     (apply #'vector-modify normal-vector normal)
                     (gl:normal (vx normal-vector)
                                (vy normal-vector)
                                (vz normal-vector)))
                   (gl:vertex (vx vertex-vector)
                              (vy vertex-vector)
                              (vz vertex-vector))))))))
