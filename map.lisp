(in-package :tradewarz)

(defclass world-map ()
  ((width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)
   (tiles :accessor tiles
          :initarg :tiles)))

(defun make-polygon (sides color)
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

(defun load-map (scene data)
  (setf (world-map scene) (apply #'make-instance 'world-map data)))

(defun generate-map ()
  (let ((world-map (world-map (scene *game*))))
    (loop for x from 0 to (1- (width world-map)) do
          (gl:push-name x)
          (loop for y from 0 to (1- (height world-map))
                for tile = (aref (tiles world-map) y x)
                for (x-pos y-pos) = `(,(* x 72) ,(* y 20)) do
                (if (evenp y)
                  (incf x-pos 36))
                (gl:with-pushed-matrix
                  (gl:rotate 90 0 0 1)
                  (gl:push-name y)
                  (draw-entity tile x-pos y-pos))))))
