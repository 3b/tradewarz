(in-package :tradewarz)

(defun make-shape (shape color)
  (case shape
    (:hexagon (make-polygon 4 color))
    (otherwise (make-quad color))))

(defun make-quad (color)
  `(((0 0 0) (0 0) ,color)
    ((0 1 0) (0 1) ,color)
    ((1 0 0) (1 0) ,color)
    ((1 1 0) (1 1) ,color)))

(defun make-polygon (sides color)
  (loop with angle = 0
        with lines = '()
        with slice = (/ (* 2 pi) sides)
        with radius = 0.5
        for vertex from 1 to sides
        for x = (/ (round (* (cos angle) 10)) 10)
        for y = (sin angle) do
        (incf angle slice)
        (push `((,(* radius x) ,(* radius y) 0)
                (,(/ (+ x 1) 2) ,(/ (+ y 1) 2) 0)
                ,color) lines)
        finally (return (push `((,radius 0 0)
                                (1 0.5 0)
                                ,color) lines))))
 
