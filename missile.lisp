(in-package #:battleship)

(defvar *missiles-fired* '())

(defclass missile ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   (radius 
    :initarg :radius
    :initform 6.0
    :accessor radius)))

(defmethod ray-intersect ((self missile) v1 v2)
  (with-slots (pos radius) self
    (let ((distance (ray-sphere-collision pos (* 2 radius) v1 v2)))
      (when distance (sb-cga:vec+ v1 (sb-cga:vec* (sb-cga:vec- v1 v2) distance))))))

(defun fire-missile (v1 v2 location)
  (let ((flag nil))
    (loop for missile in *missiles-fired* do
       ;; missile cannot be fire on top of another
	 (when (ray-intersect missile v1 v2)
	   (setf flag t)))
    (unless flag (push (make-instance 'missile :pos location) *missiles-fired*))))
