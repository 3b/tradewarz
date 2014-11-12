(in-package :tradewarz)

(defclass display ()
  ((width :accessor width
          :initarg :width
          :initform 1280)
   (height :accessor height
           :initarg :height
           :initform 800)
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

(defun configure-display ()
  (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-gl-get-proc-address
        (sdl:frame-rate) (fps *display*)))
