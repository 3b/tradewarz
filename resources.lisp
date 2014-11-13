(in-package :tradewarz)

(defun load-asset (asset)
  (loop for entity in (read-asset asset)
        for image = (getf entity :image)
        when image do (load-texture (get-path "res" image)) do
        (gl:enable :texture-2d :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:with-primitive :triangle-strip
          (loop for (object texture color) in (getf entity :lines)
                for location = (mapcar #'* object (getf entity :size)) do
                (apply #'gl:color color)
                (apply #'gl:tex-coord texture)
                (apply #'gl:vertex location)))))

(defun load-texture (resource &key (imagep t))
  ;; TODO: create non-image textures?
  (if imagep
    (surface->texture (image->surface resource))))
