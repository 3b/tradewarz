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
   (shape :reader shape
          :initarg :shape
          :initform :quad)
   (size :reader size
         :initarg :size
         :initform '(32 32 32))
   (color :reader color
          :initarg :color
          :initform '(1 1 1))
   (lines :reader lines
          :initarg :lines
          :initform nil)
   (texture-id :accessor texture-id
               :initarg :texture-id
               :initform 0)))

(defmethod initialize-instance :after ((object model) &key)
  (when (image object)
    (setf (texture-id object) (load-texture (image object)))))

(defun get-model (name)
  (gethash name (models (current-scene))))

(defmethod vertices ((model model))
  (or (lines model)
      (make-shape (get-shape model) (color model))))

(defmethod get-size ((model model))
  (if (tile model)
    (tile-size (current-map))
    (size model)))

(defmethod get-shape ((model model))
  (if (tile model)
    (tile-shape (current-map))
    (shape model)))
