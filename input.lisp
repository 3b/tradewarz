(in-package :tradewarz)

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (case key
    (:sdl-key-escape (sdl:push-quit-event))))
