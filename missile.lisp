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

(defun fire-missile (location)
  (push (make-instance 'missile :pos location) *missiles-fired*))
