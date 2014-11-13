(in-package :tradewarz)

(defmacro restartable (&body body)
  `(restart-case
     (progn ,@body)
     (continue () :report "Continue")))

(defun get-path (sub-path filename)
  (let ((path (format nil "~a/~a" sub-path filename)))
    (asdf:system-relative-pathname "tradewarz" path)))

(defun read-asset (asset)
  (let ((filename (get-path "assets" (format nil "~a.lisp" asset))))
    (with-open-file (in filename :direction :input)
      (read in))))

(defun image->surface (filename)
  (let* ((image (sdl-image:load-image filename))
         (surface (sdl:create-surface
                    (sdl:width image)
                    (sdl:height image)
                    :bpp 32
                    :pixel-alpha t)))
    (sdl:draw-surface-at-* image 0 0 :surface surface)
    surface))

(defun surface->texture (surface)
  (let ((texture (car (gl:gen-textures 1)))
        (width (sdl:width surface))
        (height (sdl:height surface)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-image-2d
      :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte
      (sdl-base::with-pixel (pixels (sdl:fp surface))
        (sdl-base::pixel-data pixels)))
    texture))
