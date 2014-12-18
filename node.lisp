(in-package :tradewarz)

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
        :initform (make-vector))))

(defun make-node (model)
  (make-instance 'scene-node :model model))

(defmethod add-child ((parent scene-node) (child scene-node))
  (setf (gethash child (children parent)) child)
  (setf (parent child) parent))

(defun add-node (node &key parent)
  "Add a scene node as a child of the given parent, or a child of the tree
   root if no parent given"
  (add-child (or parent (root (current-scene))) node))

(defun update-node (node)
  (when (or (dirtyp node)
             (rotatingp node)
             (movingp node))
    (update-local-basis node)
    (update-world-basis node)
    (setf (dirtyp node) nil)))

(defun node-world-coords (node point)
  "Return the world coordinates of a point in a node's local coordinates"
  (matrix-apply-new (world-basis node) point))

(defun pick-nodes ()
  (loop-scene #'pick-node))
 
(defun pick-node (node)
  (when (model node)
    (let* ((model (get-model (model node)))
           (size (get-size model))
           (model-origin (vector-multiply-new (make-vector 0 0 0) size))
           (radial-extent-origin (vector-multiply-new (radial-extent model) size))
           (world-model-origin (node-world-coords node model-origin))
           (world-radial-extent (node-world-coords node radial-extent-origin))
           (distance (vector-distance world-model-origin world-radial-extent)))
      (print distance))))
 
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

(defun render-node (node)
  "Render the geometry of a scene node's model each frame"
  (let ((model (get-model (model node))))
    (when model
      (gl:with-pushed-matrix
        (gl:mult-matrix (convert-to-opengl-new (world-basis node)))
        (gl:bind-texture :texture-2d (texture-id model))
        (gl:with-primitive (primitive model)
          (loop with vertex-vector = (make-vector)
                with normal-vector = (make-vector)
                with size = (get-size model)
                for (normal vertex texture color) in (geometry model)
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
