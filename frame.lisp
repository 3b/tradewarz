(in-package :tradewarz)

(defclass frame ()
  ((init :accessor init
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
  (get-internal-real-time))

(defmethod step-frame ((frame frame))
  (with-slots (init now before delta frames interval) frame
    (incf frames)
    (setf now (running-time)
          delta (- now before)
          before now)
    (let* ((seconds (/ (- now init) internal-time-units-per-second)))
      (when (and (debugp *game*)
                 (> seconds interval))
        (format t "FPS: ~,2f~%" (/ frames seconds))
        (setf frames 0
              init (running-time))))))
