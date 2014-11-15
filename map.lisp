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
          (loop for y from 0 to (1- (height world-map)) do
                (gl:with-pushed-matrix
                  (gl:translate (* (* x 32) 2) (* (* y 32) 2) 0)
                  (draw-entity (aref (tiles world-map) y x)))))))
