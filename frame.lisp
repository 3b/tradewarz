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
   (fps-interval :reader fps-interval
                 :initform 5.0)))

(defun running-time ()
  (sdl:sdl-get-ticks))

(defun step-frame ()
  (with-accessors ((start start)
                   (now now)
                   (before before)
                   (delta delta)
                   (frames frames)
                   (fps-interval fps-interval))
                  (current-scene)
    (setf now (running-time)
          delta (- now before)
          before now)
    (incf frames)
    (when (and (debugp *game*)
               (> (- now start) (* fps-interval 1000)))
      (print (/ frames fps-interval))
      (setf start (running-time)
            frames 0))))
