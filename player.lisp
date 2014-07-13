(in-package #:battleship)

(defclass player (db-object)
  ((name :initarg :name
	 :initform (error ":name required")
	 :accessor name)
   (ships)
   (pings)
   (missiles)))
