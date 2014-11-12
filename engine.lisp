(in-package :tradewarz)

(defparameter *display* (make-instance 'display))

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:idle ()
     (draw))))

(defun draw ()
  (gl:clear :color-buffer-bit)
  (gl:enable :texture-2d)
  (gl:bind-texture :texture-2d (image-resource "alien.png"))
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
