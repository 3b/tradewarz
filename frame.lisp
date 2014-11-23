(in-package :tradewarz)

(defun step-frame ()
  (loop-scene-tree (root (current-scene)))
  )

(defun update-scene-node (node)
  (print node))

(defun loop-scene-tree (parent)
  (update-scene-node parent)
  (loop for child being the hash-values of (children parent)
        do (loop-scene-tree child)))
