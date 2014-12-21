(in-package :tradewarz)

(declaim (optimize (safety 0) (space 0) (speed 3) (debug 0)))

(deftype ax-vector () '(simple-array single-float (3)))
(defstruct (ax-vector
             (:type (vector single-float))
             (:constructor make-vector (&optional x y z))
             (:conc-name v))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-copy-*))
(declaim (inline vector-copy-*))
(defun vector-copy-* (src dest)
  "Copy a vector's components to another vector"
  (psetf (vx dest) (vx src)
         (vy dest) (vy src)
         (vz dest) (vz src))
  dest)

(declaim (ftype (function (ax-vector) ax-vector) vector-copy))
(declaim (inline vector-copy))
(defun vector-copy (src)
  "Copy a vector's components to a new vector"
  (let ((dest (make-vector)))
    (psetf (vx dest) (vx src)
           (vy dest) (vy src)
           (vz dest) (vz src))
    dest))

(declaim (ftype (function (ax-vector) ax-vector) vector-clear))
(declaim (inline vector-clear))
(defun vector-clear (src)
  "Zero all components of a vector"
  (psetf (vx src) 0.0
         (vy src) 0.0
         (vz src) 0.0)
  src)

(declaim (ftype (function (ax-vector
                            &optional single-float single-float single-float)
                          ax-vector) vector-modify))
(declaim (inline vector-modify))
(defun vector-modify (src &optional x y z)
  "Assign new components to a vector"
  (psetf (vx src) (or x (vx src))
         (vy src) (or y (vy src))
         (vz src) (or z (vz src)))
  src)

(declaim (inline vector->list))
(defun vector->list (src)
  "Convert a vector to a list of its components"
  (list (vx src)
        (vy src)
        (vz src)))

(declaim (ftype (function (ax-vector) ax-vector) vector-negate-*))
(declaim (inline vector-negate-*))
(defun vector-negate-* (src)
  "Negate a vector's components"
  (psetf (vx src) (- (vx src))
         (vy src) (- (vy src))
         (vz src) (- (vz src)))
  src)

(declaim (ftype (function (ax-vector) ax-vector) vector-negate))
(defun vector-negate (src)
  "Negate a vector's components as a new vector"
  (vector-negate (vector-copy src)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-add-*))
(declaim (inline vector-add-*))
(defun vector-add-* (src1 src2 dest)
  "Store the sum of two vectors in an existing vector"
  (psetf (vx dest) (+ (vx src1) (vx src2))
         (vy dest) (+ (vy src1) (vy src2))
         (vz dest) (+ (vz src1) (vz src2)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-add))
(defun vector-add (src1 src2)
  "Store the sum of two vectors in a new vector"
  (vector-add-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-subtract-*))
(declaim (inline vector-subtract-*))
(defun vector-subtract-* (src1 src2 dest)
  "Store the difference of two vectors in an existing vector"
  (psetf (vx dest) (- (vx src1) (vx src2))
         (vy dest) (- (vy src1) (vy src2))
         (vz dest) (- (vz src1) (vz src2)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-subtract))
(defun vector-subtract (src1 src2)
  "Store the difference of two vectors in a new vector"
  (vector-subtract-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-multiply-*))
(declaim (inline vector-multiply-*))
(defun vector-multiply-* (src1 src2 dest)
  "Store the product of two vectors in an existing vector"
  (psetf (vx dest) (* (vx src1) (vx src2))
         (vy dest) (* (vy src1) (vy src2))
         (vz dest) (* (vz src1) (vz src2)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-multiply))
(defun vector-multiply (src1 src2)
  "Store the product of two vectors in a new vector"
  (vector-multiply-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector single-float) ax-vector) vector-scale-*))
(declaim (inline vector-scale-*))
(defun vector-scale-* (src scalar)
  "Scale the length of a vector"
  (psetf (vx src) (* (vx src) scalar)
         (vy src) (* (vy src) scalar)
         (vz src) (* (vz src) scalar))
  src)

(declaim (ftype (function (ax-vector single-float) ax-vector) vector-scale))
(defun vector-scale (src scalar)
  "Scale the length of a vector as a new vector"
  (vector-scale-* (vector-copy src) scalar))

(declaim (inline vector-length))
(defun vector-length (src)
  "Compute the Euclidean length of a vector"
  (sqrt (+ (* (vx src) (vx src))
           (* (vy src) (vy src))
           (* (vz src) (vz src)))))

(declaim (inline vector-normalize-*))
(defun vector-normalize-* (src)
  "Convert a vector to a unit vector"
  (let ((magnitude (vector-length src)))
    (psetf (vx src) (/ (vx src) magnitude)
           (vy src) (/ (vy src) magnitude)
           (vz src) (/ (vz src) magnitude))
    src))

(defun vector-normalize (src)
  "Convert a vector into a unit vector as a new vector"
  (vector-normalize-* (vector-copy src)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-cross-*))
(declaim (inline vector-cross-*))
(defun vector-cross-* (src1 src2 dest)
  "Compute the cross product of two vectors to an existing vector"
  (psetf (vx dest) (- (* (vy src1) (vz src2))
                      (* (vy src2) (vz src1)))
         (vy dest) (- (- (* (vx src1) (vz src2))
                         (* (vx src2) (vz src1))))
         (vz dest) (- (* (vx src1) (vy src2))
                      (* (vx src2) (vy src1))))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-cross))
(defun vector-cross (src1 src2)
  "Compute the cross product of two vectors to a new vector"
  (vector-cross-* src1 src2 (make-vector)))

(declaim (inline vector-dot))
(defun vector-dot (src1 src2)
  "Compute the dot product of two vectors"
  (+ (* (vx src1) (vx src2))
     (* (vy src1) (vy src2))
     (* (vz src1) (vz src2))))

(declaim (inline vector-distance))
(defun vector-distance (src1 src2)
  "Compute the Euclidean distance between two vectors"
  (let ((x (- (vx src2) (vx src1)))
        (y (- (vy src2) (vy src1)))
        (z (- (vz src2) (vz src1))))
    (sqrt (+ (* x x)
             (* y y)
             (* z z)))))

(declaim (inline point-line-distance))
(defun point-line-distance (start end point)
  "Calculate the shortest distance between a line and a point"
  (let ((line (vector-subtract end start))
        (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line v))
       (vector-length line))))
