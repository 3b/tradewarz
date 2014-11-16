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

(defmethod draw-tile ((shape (eql :hexagon)) x y)
  (let* ((tile (aref (tiles (current-map)) y x))
         (tile-size (tile-size (current-map)))
         (offset-x (* x (* (car tile-size) 0.75)))
         (offset-y (* y (* (cadr tile-size) (* 0.43301270189221935 2)))))
    (when (evenp x)
      (incf offset-y (* (cadr tile-size) 0.43301270189221935)))
      (apply #'draw-entity tile `(,offset-x ,offset-y))))
