(in-package :tradewarz)

(defclass model ()
  ((name :reader name
         :initarg :name)
   (image :reader image
          :initarg :image
          :initform nil)
   (tile :reader tile
         :initarg :tile
         :initform nil)
   (primitive :reader primitive
              :initarg :primitive
              :initform :triangle-strip)
   (object :reader object
           :initarg :object
           :initform nil)
   (size :reader size
         :initarg :size
         :initform nil)
   (color :reader color
          :initarg :color
          :initform '(1 1 1))
   (geometry :accessor geometry
          :initarg :geometry
          :initform nil)
   (radial-extent :accessor radial-extent
                  :initarg :radial-extent
                  :initform (make-vector 0 0 0))
   (texture-id :accessor texture-id
               :initarg :texture-id
               :initform 0)))

(defmethod initialize-instance :after ((object model) &key)
  (when (image object)
    (setf (texture-id object) (load-texture (image object)))))

(defun get-model (name)
  (gethash name (models (current-scene))))

(defmethod get-size ((model model))
  (apply #'make-vector (or (size model)
                           (tile-size (current-map)))))

(defun translate-geometry (geometry)
  (flet ((x (x)
           (etypecase x
             (list
              (make-vector (or (first x) 0) (or (second x) 0) (or (third x) 0)))
             ((simple-array single-float (3))
              x)
             (vector
              (make-vector (aref x 0) (aref x 1) (aref x 2))))))
    (loop for v in geometry
          ;; some entries don't have normals, but also don't disable
          ;; lighting so add a reasonable default normal instead of
          ;; just using last normal from previous thing drawn
          if (first v)
            collect (mapcar #'x v)
          else collect (cons (make-vector 0 1 0)
                             (mapcar #'x (cdr v))))))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)
           (if (object model)
               (setf (geometry model) (load-obj (object model)))
               (setf (geometry model) (translate-geometry (geometry model))))
           (find-radial-extent model)))

(defun find-radial-extent (model)
  (loop with farthest = 0
        with origin = (make-vector 0 0 0)
        for (nil vertex nil nil) in (geometry model)
        for distance = (vector-distance origin vertex)
        do (when (> distance farthest)
             (setf (radial-extent model) vertex
                   farthest distance))))
