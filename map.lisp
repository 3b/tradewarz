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

(defun generate-map ()
  (loop for x from 0 to (1- (width (current-map))) do
        (loop for y from 0 to (1- (height (current-map))) do
              (draw-tile (tile-shape (current-map)) x y))))

(defmethod draw-tile :around (shape x y &key size location)
  (let* ((tile (aref (tiles (current-map)) y x))
         (entity (make-entity tile :layer :map))
         (size (tile-size (current-map)))
         (location (mapcar #'* size (list x y 0)))
         (offset (call-next-method shape x y
                                   :size size
                                   :location location)))
    (move entity offset)))

(defmethod draw-tile (shape x y &key size location)
  location)

(defmethod draw-tile ((shape (eql :hexagon)) x y &key size location)
  (let* ((unit-offset (list 3/4 0.4330127 1))
         (offset (mapcar #'* location unit-offset (list 1 2 1))))
    (when (evenp x)
      (incf (cadr offset) (* (cadr size) (cadr unit-offset))))
    offset))
