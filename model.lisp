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
