(in-package :tradewarz)

(defparameter *display* (make-instance 'display))

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:idle ()
     (restartable (draw)))))

(defun draw ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha 1)
  (gl:clear :color-buffer-bit)
  (gl:enable :texture-2d)
  (gl:bind-texture :texture-2d (image-resource "alien.png"))
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 1)
    (gl:vertex -1 -1 0)
    (gl:tex-coord 1 1)
    (gl:vertex  1 -1 0)
    (gl:tex-coord 1 0)
    (gl:vertex  1  1 0)
    (gl:tex-coord 0 0)
    (gl:vertex -1  1 0))
  (gl:flush)
  (sdl:update-display))

(defun clean-up ())

(defun start-game ()
  (sdl:with-init ()
    (create-display)
    (define-events)
    (clean-up)))

(defun tradewarz ()
  (bt:make-thread #'start-game :name "tradewarz"))
