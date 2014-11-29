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

(defmethod draw-tile :around (shape x y layer)
  (let* ((tile (aref (tiles (current-map)) y x))
         (node (make-node tile))
         (offset (call-next-method shape x y layer)))
    (add-node node :parent layer)
    (vector-modify (dr node) 0 0 (/ pi 2))
    (apply #'vector-modify (dv node) offset)))

(defmethod draw-tile (shape x y layer)
  (list x y 0))

(defmethod draw-tile ((shape (eql :hexagon)) x y layer)
  (let* ((unit-offset (list 0.4330127 3/4 1))
         (location (list x y 0))
         (offset (mapcar #'* location unit-offset (list 1 2 1))))
    (when (evenp x)
      (incf (cadr offset) (cadr unit-offset)))
    offset))

(defun generate-map ()
  (let ((map-layer (make-instance 'scene-node)))
    (add-node map-layer)
    (loop for x below (width (current-map))
          do (loop for y below (height (current-map))
                   do (draw-tile (tile-shape (current-map)) x y map-layer)))))
