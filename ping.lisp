(in-package #:battleship)

(defvar *ping* nil)

(defclass ping ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   (radius 
    :initarg :radius
    :initform 1.0
    :accessor radius)))

(defmethod ray-intersect ((self ping) v1 v2)
  (with-slots (pos radius) self
    (ray-sphere-collision pos (* 2 radius) v1 v2)))

(defun make-ping (location)
  (let ((location (sb-cga:vec+ location (sb-cga:vec 0.0 0.0 -1.0))))
    (setf *ping* (make-instance 'ping :pos location))))

;; (defun place-ping ()
;;   (push *current-ping* *pings-placed*)
;;   (setf *current-ping* nil))

