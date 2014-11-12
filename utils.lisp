(in-package :tradewarz)

(defmacro restartable (&body body)
  `(restart-case
     (progn ,@body)
     (continue () :report "Continue")))
