(in-package :tradewarz)

(defclass world-map ()
  ((width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)
   (tiles :accessor tiles
          :initarg :tiles)))

(defun load-map (scene data)
  (setf (world-map scene) (apply #'make-instance 'world-map data)))

(defun generate-map ()
  (let ((world-map (world-map (scene *game*))))
    (loop for x from 0 to (1- (width world-map)) do
          (loop for y from 0 to (1- (height world-map))
                for tile = (aref (tiles world-map) y x)
                for (x-pos y-pos) = `(,(* x 96) ,(* y 27)) do
                (if (evenp y)
                  (incf x-pos 48))
                (draw-entity tile x-pos y-pos)))))
