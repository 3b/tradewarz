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
                  :initform (make-vector))
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

(defun load-models (scene asset)
  (loop for (name data) in (read-data "assets" asset)
        for model = (apply #'make-instance 'model :name name data)
        do (setf (gethash name (models scene)) model)
        (read-geometry model)
        (read-radial-extent model)))

(defstruct (geometry
             (:conc-name g))
  (n (make-array '(0) :initial-element (make-vector) :element-type 'ax-vector)
     :type (simple-array ax-vector))
  (v (make-array '(0) :initial-element (make-vector) :element-type 'ax-vector)
     :type (simple-array ax-vector))
  (uv (make-array '(0) :initial-element (make-vector) :element-type 'ax-vector)
      :type (simple-array ax-vector))
  (c (make-array '(0) :initial-element (make-vector) :element-type 'ax-vector)
     :type (simple-array ax-vector)))

(defun read-geometry (model)
  (flet ((add-vector (i v place)
           (setf (aref place i)
                 (apply #'make-vector (mapcar #'float v)))))
    (when (object model)
      (setf (geometry model) (load-obj (object model))))
    (loop with data = (geometry model)
          with size = (length data)
          with geometry = (make-geometry)
          with normals = (make-array size)
          with vertices = (make-array size)
          with uvs = (make-array size)
          with colors = (make-array size)
          with i = 0
          for (n v uv c) in data
          do (add-vector i n normals)
             (add-vector i v vertices)
             (add-vector i uv uvs)
             (add-vector i c colors)
             (incf i)
          finally (psetf (gn geometry) normals
                         (gv geometry) vertices
                         (guv geometry) uvs
                         (gc geometry) colors
                         (geometry model) geometry))))

(defun read-radial-extent (model)
  (loop with farthest = 0
        with origin = (make-vector)
        for vertex across (gv (geometry model))
        for distance = (vector-distance origin vertex)
        do (when (> distance farthest)
             (setf (radial-extent model) vertex
                   farthest distance))))
