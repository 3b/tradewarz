(in-package :tradewarz)

(defclass entity ()
  ((name :reader name
         :initarg :name)
   (image :reader image
          :initarg :image
          :initform nil)
   (shape :reader shape
          :initarg :shape
          :initform :quad)
   (size :reader size
         :initarg :size
         :initform '(32 32))
   (sides :reader sides
          :initarg :sides
          :initform nil)
   (color :reader color
          :initarg :color
          :initform '(1 1 1))
   (lines :reader lines
          :initarg :lines
          :initform nil)
   (texture-id :accessor texture-id
               :initarg :texture-id
               :initform 0)))

(defmethod initialize-instance :after ((object entity) &key)
  (when (image object)
    (setf (texture-id object) (load-texture (image object)))))

(defun entity (name)
  (gethash name (entities (current-scene))))

(defmethod primitive ((entity entity))
  (if (eq (size entity) :map-tile)
    :triangle-fan
    :triangle-strip))

(defmethod vertices ((entity entity))
  (or (lines entity)
      (make-shape (shape entity) (color entity))))

(defmethod get-size ((entity entity))
  (if (eq (size entity) :map-tile)
    (tile-size (current-map))
    (size entity)))

(defun draw-entity (name x y)
  (let* ((entity (entity name))
         (size (get-size entity)))
    (gl:with-pushed-matrix
      (gl:bind-texture :texture-2d (texture-id entity))
      (gl:translate x y 0)
      (gl:with-primitive (primitive entity)
        (loop for (object texture color) in (vertices entity) do
              (apply #'gl:color color)
              (apply #'gl:tex-coord texture)
              (apply #'gl:vertex (mapcar #'* object size)))))))
