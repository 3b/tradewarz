(in-package :tradewarz)

(defun load-asset (asset)
  (let* ((data (read-asset asset)))
    (loop for shape in (getf data :shapes)
          for image = (getf shape :image)
          for size = (getf shape :size)
          when image do (load-texture (get-path "res" image))
          do (gl:with-primitive :polygon
               (loop for (object texture color) in (getf shape :lines) do
                     (apply #'gl:color color)
                     (apply #'gl:tex-coord texture)
                     (apply #'gl:vertex (mapcar #'* object size)))))))

(defun load-texture (resource &key (imagep t))
  ;; TODO: create non-image textures?
  (if imagep
    (surface->texture (image->surface resource))))
