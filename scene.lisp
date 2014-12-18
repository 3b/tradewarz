(in-package :tradewarz)

(defclass scene (frame)
  ((name :reader name
         :initarg :name)
   (root :reader root
         :initarg :root
         :initform (make-instance 'scene-node))
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

(defun current-scene ()
  (scene *game*))

(defun load-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (generate-map)

  ;; test entities
  (defparameter *e1* (make-node :tank))
  (let ((axes (make-node :axes))
        (tank (make-node :tank))
        (jet (make-node :jet)))

    (add-node axes)
    (vector-modify (dv axes) -64 32 0)

    (add-node tank)
    (setf (rotatingp tank) t)
    (vector-modify (drv tank) 0 0 0.01)
    (vector-modify (dv tank) 0 0 8)

    (add-node jet)
    (setf (rotatingp jet) t)
    (vector-modify (drv jet) 0 0 0.01)
    (vector-modify (dv jet) 56 0 32)))

(defun make-node (model)
  (let ((node (make-instance 'scene-node :model model)))
    node))

(defmethod add-child ((parent scene-node) (child scene-node))
  (setf (gethash child (children parent)) child)
  (setf (parent child) parent))

(defun add-node (node &key parent)
  (add-child (or parent (root (current-scene))) node)) 

(defun loop-scene (func &optional parent)
  (let ((parent (or parent (root (current-scene)))))
    (funcall func parent)
    (loop for child being the hash-values of (children parent)
          do (loop-scene func child))))

(defun update-scene ()
  (loop-scene #'update-node)
  (loop-scene #'render-node))

(defun update-node (node)
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

(defun render-node (node)
  (let ((model (get-model (model node))))
    (when model
      (gl:with-pushed-matrix
        (gl:mult-matrix (convert-to-opengl-new (world-basis node)))
        (gl:bind-texture :texture-2d (texture-id model))
        (gl:with-primitive (primitive model)
          (loop with vertex-vector = (make-vector)
                with normal-vector = (make-vector)
                with size = (apply #'make-vector (get-size model))
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
