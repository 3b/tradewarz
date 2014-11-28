(in-package :tradewarz)

(defun make-shape (shape color)
  (case shape
    (:hexagon (make-polygon 6 color))
    (otherwise (make-quad color))))

(defun make-quad (color)
  `(((-0.5 -0.5 0) (0 0) ,color)
    ((-0.5 0.5 0) (0 1) ,color)
    ((0.5 -0.5 0) (1 0) ,color)
    ((0.5 0.5 0) (1 1) ,color)))

(defun make-polygon (sides color)
  (nreverse
    (loop with radius = 0.5
          repeat (1+ sides)
          for slice by (/ sides)
          for angle = (* slice 2 pi)
          for x = (cos angle)
          for y = (sin angle)
          for object = (list (* radius (- x radius))
                             (* radius (- y radius)))
          for texture = (list (/ (+ x 1) 2)
                              (/ (+ y 1) 2))
          collect (list object texture color))))
