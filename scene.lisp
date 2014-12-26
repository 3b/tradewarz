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

(defmethod initialize-instance :after ((object scene) &key)
  (loop with data = (read-data "scenes" (name object))
        with world = (getf data :world)
        for asset in (getf data :assets)
        do (load-models object asset)
           (apply #'load-map object world)))

(defun current-scene ()
  (scene *game*))

(defun load-scene (game &key name)
  (sdl2:in-main-thread ()
    (setf (scene game) (make-instance 'scene :name name))
    (generate-map)

    ;; test entities
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
      (vector-modify (dv jet) 56 0 32))))

(defun loop-scene (func &optional parent)
  (let ((parent (or parent (root (current-scene)))))
    (funcall func parent)
    (loop for child being the hash-values of (children parent)
          do (loop-scene func child))))

(defun update-scene ()
  (loop-scene #'update-node)
  (loop-scene #'render-node)
  (gl:bind-vertex-array 0))


