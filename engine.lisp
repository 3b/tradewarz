(in-package :tradewarz)

(defparameter *display* (make-instance 'display))

(defun define-events ()
  (sdl:with-events ()
    (:quit-event () t)
    (:idle ()
     (restartable (draw)))))

(defun draw ()
  (gl:clear :color-buffer-bit)
  (load-asset "alien")
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
