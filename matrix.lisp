(in-package :tradewarz)

(defstruct (ax-matrix
             (:constructor make-matrix (&optional m00 m01 m02 m03
                                                  m10 m11 m12 m13
                                                  m20 m21 m22 m23
                                                  m30 m31 m32 m33))
             (:conc-name nil)
             (:print-function print-matrix))
  (m00 0.0)
  (m01 0.0)
  (m02 0.0)
  (m03 0.0)
  (m10 0.0)
  (m11 0.0)
  (m12 0.0)
  (m13 0.0)
  (m20 0.0)
  (m21 0.0)
  (m22 0.0)
  (m23 0.0)
  (m30 0.0)
  (m31 0.0)
  (m32 0.0)
  (m33 0.0))

(defun print-matrix (struct stream depth)
  (declare (ignore depth))
  (with-matrix (m struct)
    (print-unreadable-object (struct stream)
      (format
        stream "~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a"
        m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))))

(defmacro with-matrix ((prefix matrix) &body body)
  `(with-accessors ((,(alexandria:symbolicate prefix "00") m00)
                    (,(alexandria:symbolicate prefix "01") m01)
                    (,(alexandria:symbolicate prefix "02") m02)
                    (,(alexandria:symbolicate prefix "03") m03)
                    (,(alexandria:symbolicate prefix "10") m10)
                    (,(alexandria:symbolicate prefix "11") m11)
                    (,(alexandria:symbolicate prefix "12") m12)
                    (,(alexandria:symbolicate prefix "13") m13)
                    (,(alexandria:symbolicate prefix "20") m20)
                    (,(alexandria:symbolicate prefix "21") m21)
                    (,(alexandria:symbolicate prefix "22") m22)
                    (,(alexandria:symbolicate prefix "23") m23)
                    (,(alexandria:symbolicate prefix "30") m30)
                    (,(alexandria:symbolicate prefix "31") m31)
                    (,(alexandria:symbolicate prefix "32") m32)
                    (,(alexandria:symbolicate prefix "33") m33))
     ,matrix
     ,@body))

(defmacro with-matrices (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(with-matrix ,(car binds)
       (with-matrices ,(cdr binds) ,@body))))

(defun matrix-copy (src dest)
  "Copy a matrix to an existing matrix"
  (with-matrices ((a src) (b dest))
    (psetf b00 a00 b01 a01 b02 a02 b03 a03
           b10 a10 b11 a11 b12 a12 b13 a13
           b20 a20 b21 a21 b22 a22 b23 a23
           b30 a30 b31 a31 b32 a32 b33 a33))
  dest)

(defun matrix-copy-new (src)
  "Copy a matrix to a new matrix"
  (matrix-copy src (make-matrix)))

(defun matrix-identity (src)
  "Set a matrix to the identity matrix"
  (with-matrix (m src)
    (psetf m00 1 m01 0 m02 0 m03 0
           m10 0 m11 1 m12 0 m13 0
           m20 0 m21 0 m22 1 m23 0
           m30 0 m31 0 m32 0 m33 1))
  src)

(defun matrix-identity-new ()
  "Create a new identity matrix"
  (matrix-identity (make-matrix)))

(defun matrix-multiply (src1 src2 dest)
  "Store the product of two matrices in an existing matrix"
  (with-matrices ((a src1) (b src2) (c dest))
    (psetf c00 (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
           c10 (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
           c20 (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
           c30 (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))
           c01 (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31))
           c11 (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31))
           c12 (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31))
           c13 (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31))
           c20 (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32))
           c21 (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32))
           c22 (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32))
           c23 (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32))
           c30 (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))
           c31 (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))
           c32 (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))
           c33 (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))))
  dest)

(defun matrix-multiple-new (src1 src2)
  "Store the product of two matrices in a new matrix"
  (matrix-multiply src1 src2 (make-matrix)))
