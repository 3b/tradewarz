(in-package :tradewarz)

(defclass world-map ()
  ((width :reader width
          :initarg :width)
   (height :reader height
           :initarg :height)
   (tile-shape :reader tile-shape
               :initarg :tile-shape
               :initform :quad)
   (tile-size :reader tile-size
              :initarg :tile-size
              :initform '(64 64))
   (tiles :reader tiles
          :initarg :tiles)))

(defun current-map ()
  (world-map (current-scene)))

(defun load-map (scene data)
  (setf (world-map scene) (apply #'make-instance 'world-map data)))

(defun find-tile (tile-id)
  (loop with models = (models (current-scene))
        for tile being the hash-keys of models
        for model = (gethash tile models)
        do (when (= tile-id (tile model))
             (return tile))))

(defmethod draw-tile :around (shape x y &key layer)
  (let* ((tile-id (aref (tiles (current-map)) y x))
         (node (make-node (find-tile tile-id)))
         (offset (call-next-method shape x y :node node)))
    (add-node node :parent layer)
    (apply #'vector-modify (dv node) offset)))

(defmethod draw-tile (shape x y &key)
  (list x y 0))

(defmethod draw-tile ((shape (eql :hexagon)) x y &key)
  (let* ((unit-offset (list 0.4330127 3/4 1))
         (location (list x y 0))
         (offset (mapcar #'* location unit-offset (list 1 2 1))))
    (when (evenp x)
      (incf (cadr offset) (cadr unit-offset)))
    offset))

(defmethod draw-tile :after ((shape (eql :hexagon)) x y &key node)
  (vector-modify (dr node) 0 0 (/ pi 2)))

(defun generate-map ()
  (let ((layer (make-instance 'scene-node))
        (shape (tile-shape (current-map))))
    (add-node layer)
    (loop for x below (width (current-map))
          do (loop for y below (height (current-map))
                   do (draw-tile shape x y :layer layer)))))
