(in-package #:battleship)

(defclass ping ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   (radius 
    :initarg :radius
    :initform 1.0
    :accessor radius)))


;; used by client gui to determine radius 
(defun make-ping (location)
  (let ((location (sb-cga:vec+ location (sb-cga:vec 0.0 0.0 -1.0))))
    (setf *ping* (make-instance 'ping :pos location))))

(defvar *ping* nil)
(defvar *ping-hits* '())
