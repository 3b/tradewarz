(in-package :tradewarz)

(defmethod draw-tile ((shape (eql :hexagon)) x y &key)
  "Calculate the offset a hexagon is to be drawn at given its coordinates."
  (let* ((unit-offset (list 0.4330127 3/4 1))
         (location (list x y 0))
         (offset (mapcar #'* location unit-offset (list 1 2 1))))
    (when (oddp x)
      (incf (cadr offset) (cadr unit-offset)))
    offset))

(defmethod draw-tile :after ((shape (eql :hexagon)) x y &key node)
  "Rotate a hexagon 90 degrees immediately after it is drawn, in order to
   have the flat edges on the left and right sides rather than top and bottom"
  (vector-modify (dr node) 0 0 (/ pi 2)))

(defun axial->cube (src)
  "Convert a 2-D vector representing axial coordinates to a 3-D vector
   representing cube coordinates"
  (let* ((x (vy src))
         (z (vx src))
         (y (- (- x) z)))
    (make-vector x y z)))

(defun cube->axial (src)
  "Convert a 3-D vector representing cube coordinates to a 2-D vector
   representing axial coordinates"
  (make-vector (vz src) (vx src)))

(defun cube->hex (src)
  "Convert a 3-D vector representing cube coordinates to a 2-D vector
   representing hexagon offset coordinates"
  (let* ((y (vz src))
         (x (+ (vx src) (/ (- y (mod y 2)) 2))))
    (make-vector x y)))

(defun hex->cube (src)
  "Convert a 2-D vector representing hexagon offset coordinates to a 3-D vector
   representing cube coordinates"
  (let* ((z (vy src))
         (x (- (vx src) (/ (- z (mod z 2)) 2)))
         (y (- (- x) z)))
    (make-vector x y z)))

(defun hex-distance (src dest)
  "Calculate the distance in tiles from a pair of vectors representing
   hexagon offset coordinates"
  (let* ((src (hex->cube src))
         (dest (hex->cube dest))
         (x (abs (- (vx src) (vx dest))))
         (y (abs (- (vy src) (vy dest))))
         (z (abs (- (vz src) (vz dest)))))
    (max x y z)))

(defun hex-round (src)
  "Calculate the nearest hexagon from a 3-D vector respresenting cube
   coordinates"
  (let* ((src (vector->list src))
         (rounded (mapcar #'round src))
         (diff (apply #'make-vector (mapcar #'abs (mapcar #'- rounded src))))
         (dest (apply #'make-vector rounded)))
    (cond
      ((and (> (vx diff) (vy diff))
            (> (vx diff) (vz diff)))
       (setf (vx dest) (- (- (vy dest)) (vz dest))))
      ((> (vy diff) (vz diff))
       (setf (vy dest) (- (- (vx dest)) (vz dest))))
      (t (setf (vz dest) (- (- (vx dest)) (vy dest)))))
    dest))

(defun hex-neighbor (src direction)
  "Calculate the coordinates of a hexagon's neighbor given a source hexagon
   coordinate and a 2-D directional vector.
   Example: Hexagon 2,2 with direction -1,1 (northwest) would return the
   coordinates 1,1"
  (let* ((src (vector->list (hex->cube src)))
         (directions '((1 -1 0) (1 0 -1) (0 1 -1)
                       (-1 1 0) (-1 0 1) (0 -1 1)))
         (angle (atan (vy direction) (vx direction)))
         (index (mod (+ 6 (round (/ (* 6 angle) (* pi 2)))) 6))
         (offset (elt directions index))
         (dest (apply #'make-vector (mapcar #'+ src offset))))
    (cube->hex dest)))
