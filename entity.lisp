(in-package :tradewarz)

(defclass entity ()
  ((name :reader name
         :initarg :name)
   (size :reader size
         :initarg :size)
   (image :reader image
          :initarg :image
          :initform nil)
   (lines :reader lines
          :initarg :lines)
   (texture-id :accessor texture-id
               :initarg :texture-id
               :initform 0)
   (location :reader location
             :initarg :location
             :initform '(200 200))))

(defmethod initialize-instance :after ((object entity) &key)
  (when (image object)
    (setf (texture-id object) (load-texture (image object)))))

(defun draw-entity (name)
  (let ((entity (gethash name (entities (scene *game*)))))
    (gl:bind-texture :texture-2d (texture-id entity))
    (gl:with-primitive :triangle-strip
      (loop for (object texture color) in (lines entity) do
            (apply #'gl:color color)
            (apply #'gl:tex-coord texture)
            (apply #'gl:vertex (mapcar #'* object (size entity)))))))

(defun load-texture (texture)
  (let* ((resource (get-path "res" texture))
         (texture-id (gethash resource (textures *game*))))
    (or texture-id
        (setf (gethash resource (textures *game*))
              (surface->texture (image->surface resource))))))
