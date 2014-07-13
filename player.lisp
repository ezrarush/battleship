(in-package #:battleship)

(defvar *players* '())

(defclass player (db-object)
  ((name :initarg :name
	 :initform (error ":name required")
	 :accessor name)
   (ships :initarg :ships
	  :initform (error ":ships required")
	  :accessor ships)
   (energy :initarg :energy
	  :initform (error ":pings required")
	  :accessor energy)
   (missiles :initarg :missiles
	     :initform (error ":missiles required")
	     :accessor missiles)
   (opponent :initarg :opponent
	     :initform (error ":opponent required")
	     :accessor opponent)))
