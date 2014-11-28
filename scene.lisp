(in-package :tradewarz)

(defclass scene ()
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

(defun load-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (generate-map)

  ;; test entities
  (defparameter *e1* (make-node :alien-small))
  
    (add-node *e1*)
    (setf (movingp *e1*) t)
    (setf (rotatingp *e1*) t)
    (vector-modify (dv *e1*) 1 1 -1)
    (vector-modify (dr *e1*) 0 0 0)
    )

(defun current-scene ()
  (scene *game*))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)))

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
  (loop-scene #'update-node))

(defun update-node (node)
  (update-local-basis node)
  (update-world-basis node))

(defun update-world-basis (node)
  (if (parent node)
    (matrix-multiply
      (world-basis (parent node))
      (local-basis node)
      (world-basis node))))

(defun update-local-basis (node)
  (matrix-translate (dv node) (local-basis node))
  (vector-clear (dv node))
  (matrix-rotate (dr node) (local-basis node))
  (vector-clear (dr node))
  (when (movingp node)
    (matrix-translate (dtv node) (local-basis node)))
  (when (rotatingp node)
    (matrix-rotate (drv node) (local-basis node)))
  )

(defun render-scene ()
  (loop-scene #'render-node))

(defun render-node (node)
  (let ((model (get-model (model node))))
    (when model
      (gl:bind-texture :texture-2d (texture-id model))
      (gl:with-primitive (primitive model)
        (loop with vertex = (make-vector)
              with size = (get-size model)
              for (object texture color) in (vertices model)
              do (apply #'gl:color color)
                 (apply #'gl:tex-coord texture)
                 (apply #'vector-modify vertex object)
                 (matrix-apply (world-basis node) vertex vertex)
                 (apply #'gl:vertex
                        (mapcar #'* (vector->list vertex) size)))))))
