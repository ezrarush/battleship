(in-package #:battleship)

(defclass camera ()
  ((pos
    :initarg :pos
    :initform (sb-cga:vec 0.0 0.0 0.0)
    :accessor camera-pos)
   (target
    :initarg :target
    :initform (sb-cga:vec 0.0 0.0 1.0)
    :accessor camera-target)
   (up
    :initarg :up
    :initform (sb-cga:vec 0.0 1.0 0.0)
    :accessor camera-up)))
