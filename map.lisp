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

(defun hex-tile-offset (world-map x y)
  (let* ((tile (aref (tiles world-map) y x))
         (tile-size (tile-size world-map))
         (offset-x (* x (* (car tile-size) 3/2)))
         (offset-y (* y (* (cadr tile-size) 5/12))))
    (when (evenp y)
      (incf offset-x (* (car tile-size) 3/4)))
    (gl:with-pushed-matrix
      (gl:rotate 90 0 0 1)
      (apply #'draw-entity tile `(,offset-x ,offset-y)))))

(defun load-map (scene data)
  (setf (world-map scene) (apply #'make-instance 'world-map data)))

(defun generate-map ()
  (let ((world-map (current-map)))
    (loop for x from 0 to (1- (width world-map)) do
          (loop for y from 0 to (1- (height world-map)) do
                (hex-tile-offset world-map x y)))))
