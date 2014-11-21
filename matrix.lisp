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

(defun print-matrix (struct stream depth)
  (declare (ignore depth))
  (with-matrix (m struct)
    (print-unreadable-object (struct stream)
      (format
        stream "~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a"
        m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))))
