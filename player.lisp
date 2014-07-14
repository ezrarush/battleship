(in-package #:battleship)

(defvar *players* nil)

(defclass player ()
  ((connection 
    :initarg :socket-connection
    :accessor socket-connection)
   (name 
    :initarg :name
    ;; :initform (error ":name required")
    :accessor name)
   (ships 
    :initarg :ships
    ;; :initform (error ":ships required")
    :accessor ships)
   (energy 
    :initarg :energy
    ;; :initform (error ":pings required")
    :accessor energy)
   (missiles 
    :initarg :missiles
    ;; :initform (error ":missiles required")
    :accessor missiles)
   (opponent 
    :initarg :opponent
    ;; :initform (error ":opponent required")
    :accessor opponent)
   (placed-ships
    :initform nil
    :accessor placed-ships)))

(defun make-player (socket-connection)
  (push (make-instance 'player :socket-connection socket-connection) *players*))
