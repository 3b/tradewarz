(in-package :tradewarz)

(defclass obj-file ()
  ((vertices :accessor vertices
             :initform (obj-file-array))
   (faces :accessor faces
          :initform (obj-file-array))))

(defun obj-file-array ()
  (make-array 1 :fill-pointer 1 :adjustable t :initial-element '(0 0 0)))

(defun load-obj (obj-file)
  (let ((resource (get-path "res" obj-file))
        (obj (make-instance 'obj-file)))
    (with-open-file (in resource)
      (loop for line = (remove #\Return (read-line in nil nil))
            while line
            for (name . data) = (split-sequence #\Space line)
            if (string= name "v")
            do (set-obj-vertices obj data)
            ))
    obj))

(defun set-obj-vertices (obj data)
  (let ((vertices (format nil "~{~a ~}" data)))
    (vector-push-extend
      (with-input-from-string (in vertices)
        (loop for vertex = (read in nil nil)
              while vertex
              collect vertex)) (vertices obj))))
