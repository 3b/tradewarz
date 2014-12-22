(in-package :tradewarz)

(defclass obj-file ()
  ((vertices :accessor vertices
             :initform (obj-file-array))
   (textures :accessor textures
             :initform (obj-file-array))
   (normals :accessor normals
            :initform (obj-file-array))
   (faces :accessor faces
          :initform nil)))

(defun obj-file-array ()
  (make-array 1
              :fill-pointer 1
              :adjustable t
              :initial-element (make-vector)))

(defun load-obj (obj-file)
  (let ((resource (get-path "res" (format nil "~a.obj" obj-file)))
        (obj (make-instance 'obj-file)))
    (with-open-file (in resource)
      (loop for line = (remove #\Return (read-line in nil nil))
            while line
            for (name . data) = (split-sequence #\Space line)
            if (string= name "v")
            do (set-coords 'vertices obj data)
            if (string= name "vt")
            do (set-coords 'textures obj data)
            if (string= name "vn")
            do (set-coords 'normals obj data)
            if (string= name "f")
            do (set-faces obj data)))
    (faces obj)))

(defmethod set-coords (coord-type (obj obj-file) data)
  (let ((coords (format nil "~{~a ~}" data)))
    (vector-push-extend
      (with-input-from-string (in coords)
        (apply #'make-vector
               (loop for coord = (read in nil nil)
                     while coord
                     collect coord)))
      (funcall coord-type obj))))

(defmethod set-faces ((obj obj-file) data)
  (loop for face in data
        for (vi ti ni) = (mapcar #'read-from-string (split-sequence #\/ face))
        for vertex = (aref (vertices obj) vi)
        for texture = (aref (textures obj) ti)
        for normal = (aref (normals obj) ni)
        do (push (list
                  normal
                  vertex
                  texture
                  (make-vector 1 1 1))
                 (faces obj))))
