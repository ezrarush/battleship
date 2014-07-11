(in-package #:battleship)

(defvar *placed-ships* '())

(defclass ship ()
  ((pos 
    :initarg :pos
    :initform (error ":pos required")
    :accessor pos)
   ;; orientation may be either orientation :horizontal or :vertical 
   (orientation
    :initarg :orientation
    :initform :vertical
    :accessor orientation)
   (width
    :initarg :width
    :initform 12.0
    :accessor width)
   (height
    :initarg :height
    :initform 48.0
    :accessor height)))

(defmethod (setf orientation) :after (symb (self ship))
  (with-slots (width height) self
    (rotatef width height)))


(defun place-ship (location)
  (push (make-instance 'ship :pos location) *placed-ships*))

(defun remove-ship (ship)
  (setf *placed-ships* (remove ship *placed-ships*)))
