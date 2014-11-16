(in-package :tradewarz)

(defclass world-map ()
  ((width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)
   (tile-size :accessor tile-size
              :initarg :tile-size)
   (tiles :accessor tiles
          :initarg :tiles)))

(defun current-map ()
  (world-map (current-scene)))

(defun make-polygon (shape color)
  (let ((sides
          (case shape
            (:hexagon 6)
            (otherwise 4))))
    (polygon-vertices sides color)))

(defun polygon-vertices (sides color)
  (loop with angle = 0
        with lines = '()
        with slice = (/ (* 2 pi) sides)
        with radius = 0.5
        for vertex from 1 to sides
        for x = (cos angle)
        for y = (sin angle) do
        (incf angle slice)
        (push `((,(* radius x) ,(* radius y) 0)
                (,(/ (+ x 1) 2) ,(/ (+ y 1) 2) 0)
                ,color) lines)
        finally (return (push `((,radius 0 0)
                                (1 0.5 0)
                                ,color) lines))))

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
