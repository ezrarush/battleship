(in-package #:battleship)

(defclass game-state ()
  ((board-size :initarg :board-size
	       :accessor game-state-board-size)
   (ships :initarg :ships
	  :accessor game-state-ships)
   (energy :initarg :energy
	   :accessor game-state-energy)
   (missiles :initarg :missiles
	     :accessor game-state-missiles)
   (opponent :initarg :opponent
	     :accessor game-state-opponent)
   (current-screen :initform :waiting-for-opponent
		    :accessor game-state-current-screen)))

;; slots must be bound or (userial:unserialize :game-state-from-welcome :welcome-state *game-state*) will crash
(defvar *game-state* (make-instance 'game-state :board-size 40 :ships 5 :energy 10.0 :missiles 20 :opponent "your-daddy"))

(defun make-game-state ()
  (make-instance 'game-state))


