(in-package #:battleship)

(defclass player (db-object)
  ((connection 
    :initarg :socket-connection
    :accessor socket-connection)
   (state
    :initform :new
    :accessor state)
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
   (opponent-name
    :initarg :opponent-name
    :accessor opponent-name)
   (opponent-id 
    :initarg :opponent-id
    ;; :initform (error ":opponent required")
    :accessor opponent-id)
   (placed-ships
    :initform '()
    :accessor placed-ships)))

(defun make-player (socket-connection)
  (make-instance 'player :socket-connection socket-connection))
