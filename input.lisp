(in-package :tradewarz)

(defun key-down (key state mod-key scancode unicode)
  (declare (ignore state mod-key scancode unicode))
  (case key
    (:sdl-key-escape (sdl:push-quit-event))
    (:sdl-key-left (gl:translate 5 0 0))
    (:sdl-key-right (gl:translate -5 0 0))
    (:sdl-key-up (gl:translate 0 5 0))
    (:sdl-key-down (gl:translate 0 -5 0))
    (:sdl-key-a (move (get-entity 226) -5 0 0))
    (:sdl-key-d (move (get-entity 226) 5 0 0))
    (:sdl-key-w (move (get-entity 226) 0 -5 0))
    (:sdl-key-s (move (get-entity 226) 0 5 0))))