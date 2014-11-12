(in-package :tradewarz)

(defun load-asset (asset)
  (let* ((data (read-asset asset)))
    (loop for shape in (getf data :shapes)
          for image = (getf shape :image)
          for coords = (getf shape :texture-coords)
          for lines = (getf shape :lines)
          for (width height) = (getf shape :size)
          when (and image coords) do (load-texture (get-path "res" image))
          do (gl:with-primitive :polygon
               (loop for line in lines
                     do (when coords
                          (let ((coord (pop coords)))
                            (gl:tex-coord (car coord)
                                          (cadr coord))))
                     (gl:vertex (* (car line) width)
                                (* (cadr line) height)
                                (caddr line)))))))

(defun load-texture (resource &key (imagep t))
  ;; TODO: create non-image textures?
  (if imagep
    (surface->texture (image->surface resource))))
