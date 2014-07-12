(in-package #:battleship)

(defvar *pings-placed* '())

(defclass ping ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   (radius 
    :initarg :radius
    :initform 6.0
    :accessor radius)))

(defmethod ray-intersect ((self ping) v1 v2)
  (with-slots (pos radius) self
    (let ((distance (ray-sphere-collision pos (* 2 radius) v1 v2)))
      (when distance (sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance))))))

(defun place-ping (location)
  (push (make-instance 'ping :pos location) *pings-placed*))
