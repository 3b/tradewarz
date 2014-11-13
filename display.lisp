(in-package :tradewarz)

(defclass display ()
  ((width :accessor width
          :initarg :width
          :initform 800)
   (height :accessor height
           :initarg :height
           :initform 600)
   (title :reader title
          :initform "TradeWarz")
   (fps :reader fps
        :initform 60)))

(defun create-display ()
  (let ((width (width *display*))
        (height (height *display*)))
    (make-window width height)
    (configure-display)))

(defun make-window (width height)
  (sdl:window width
              height
              :flags sdl:sdl-opengl
              :resizable t
              :double-buffer t
              :title-caption (title *display*)
              :icon-caption (title *display*)))

(defun setup-viewport (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (gl:ortho 0 width 0 height -1 0) ;; works without this line
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun configure-display ()
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address
        (sdl:frame-rate) (fps *display*))
  (setup-viewport (width *display*) (height *display*)))
