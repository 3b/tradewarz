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
            do (cond
                 ((string= name "v") (set-coords 'vertices obj data))
                 ((string= name "vt") (set-coords 'textures obj data))
                 ((string= name "vn") (set-coords 'normals obj data))
                 ((string= name "f") (set-faces obj data)))))
    (faces obj)))

(defmethod set-coords (coord-type (obj obj-file) data)
  (let ((coords (format nil "~{~a ~}" data)))
    (vector-push-extend
      (with-input-from-string (in coords)
        (apply #'make-vector
               (loop for coord = (read in nil nil)
                     for c = (when (< coord ))
                     while coord
                     collect coord)))
      (funcall coord-type obj))))

(defmethod set-faces ((obj obj-file) data)
  (loop for face in data
        for (v uv n) = (mapcar #'read-from-string (split-sequence #\/ face))
        for vertex = (aref (vertices obj) v)
        for texture = (aref (textures obj) uv)
        for normal = (aref (normals obj) n)
        do (push (list
                   (vector->list normal)
                   (vector->list vertex)
                   (vector->list texture)
                   '(1 1 1))
                 (faces obj))))
