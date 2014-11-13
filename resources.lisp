(in-package :tradewarz)

(defun load-assets ()
  (loop for asset in *assets* do
        (load-entities asset)))

(defun load-entities (asset)
  (loop for entity in (read-asset asset)
        for name = (getf entity :name)
        for image = (getf entity :image) do
        (when image
          (setf (getf entity :texture-id) (load-texture image)))
        (setf (getf *entities* name) entity)))

(defun draw-entity (entity)
  (let* ((data (getf *entities* entity))
         (texture-id (or (getf data :texture-id) 0)))
    (gl:enable :texture-2d :blend)
    (gl:bind-texture :texture-2d texture-id) 
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:with-primitive :triangle-strip
      (loop for (object texture color) in (getf data :lines)
            for location = (mapcar #'* object (getf data :size)) do
            (apply #'gl:color color)
            (apply #'gl:tex-coord texture)
            (apply #'gl:vertex location)))))

(defun load-texture (texture)
  (let ((resource (get-path "res" texture)))
    (surface->texture (image->surface resource))))
