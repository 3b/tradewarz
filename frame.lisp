(in-package :tradewarz)

(defclass frame ()
  ((start :accessor start
          :initform (running-time))
   (now :accessor now
        :initform (running-time))
   (before :accessor before
           :initform (running-time))
   (delta :accessor delta
          :initform 0)
   (frames :accessor frames
           :initform 0)
   (interval :reader interval
             :initform 5.0)))

(defun running-time ()
  (sdl:sdl-get-ticks))

(defmethod step-frame ((frame frame))
  (with-slots (start now before delta frames interval) frame
    (incf frames)
    (setf now (running-time)
          delta (- now before)
          before now)
    (let* ((seconds (/ (- now start) 1000))
           (fps (/ frames seconds))
           (ms (* 1000 (/ fps))))
      (when (and (debugp *game*)
                 (> seconds interval))
        (format t "FPS: ~,2f, ms/frame: ~,4f~%" fps ms)
        (setf frames 0
              start (running-time))))))
