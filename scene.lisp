(in-package :tradewarz)

(defclass scene ()
  ((name :reader name
         :initarg :name)
   (root :reader root
         :initarg :root
         :initform (make-instance 'scene-node))
   (world-map :accessor world-map)
   (models :reader models
             :initform (make-hash-table))
   (layer-order :reader layer-order
                :initform '(:map :mob))
   (layers :reader layers
           :initform (make-hash-table))))

(defclass scene-node (entity)
  ((parent :accessor parent
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

(defmethod print-object ((object scene) stream)
  (let ((entity-count 0))
    (loop for layer being the hash-value of (layers object)
          do (incf entity-count (length layer)))
    (format stream "Scene: ~:(~a~) (entities: ~a)"
            (name object)
            entity-count)))

(defun load-scene (&key name)
  (setf (scene *game*) (make-instance 'scene :name name))
  (make-layers)
  (generate-map)

  ;; test entities
  (let ((e1 (make-entity :alien-small
                         :layer :mob))
        (e2 (make-entity :alien-small
                         :layer :mob))
        (e3 (make-entity :alien-small
                         :layer :mob)))
    (add-node e1)
    (add-node e2 :parent e1)
    (add-node e3)
    (setf (movingp e1) t)
    (setf (movingp e2) t)
    (vector-modify (dv e2) -1 0 0)))

(defun current-scene ()
  (scene *game*))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)))

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
  (when (movingp node)
    (matrix-translate (dtv node) (local-basis node))))

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
