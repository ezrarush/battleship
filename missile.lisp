(in-package #:battleship)

(defvar *missiles-fired* '())
(defvar *current-missile* nil)
(defvar *enemy-fire* '())

(defclass missile ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   (radius 
    :initarg :radius
    :initform 2.0
    :accessor radius)
   (hit-p
    :initarg :hit-p
    :initform nil
    :accessor hit-p)))

(defmethod ray-intersect ((self missile) v1 v2)
  (with-slots (pos radius) self
    (ray-sphere-collision pos (* 2 radius) v1 v2)))

(defun fire-missile (v1 v2 location)
  (let ((location (sb-cga:vec+ location (sb-cga:vec 0.0 0.0 -3.0)))
	(flag nil))
    (loop for missile in *missiles-fired* do
       ;; missile cannot be fire on top of another
	 (when (ray-intersect missile v1 v2)
	   (setf flag t)))
    (unless flag 
      (setf *current-missile* (make-instance 'missile :pos location))
      (send-message *server-connection* (make-fire-message (aref location 0) (aref location 1))))))
