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
  (or (size model) (tile-size (current-map))))

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)
           (when (object model)
             (setf (geometry model) (load-obj (object model))))
           (find-radial-extent model)))

(defun find-radial-extent (model)
  (loop with farthest = 0
        with origin = (make-vector 0 0 0)
        for (nil v nil nil) in (geometry model)
        for vertex = (apply #'make-vector v)
        for distance = (vector-distance origin vertex)
        do (when (> distance farthest)
             (setf (radial-extent model) vertex
                   farthest distance))))
