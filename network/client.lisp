(in-package #:battleship)

(defvar *server-connection* nil)

(defun connect-to-server (server-ip port)
  (setf *server-connection*
	(usocket:socket-connect server-ip
				port
				:element-type '(unsigned-byte 8))))
(defun disconnect-from-server ()
  (assert *server-connection*)
  (usocket:socket-close *server-connection*)
  (setf *server-connection* nil))


(userial:make-accessor-serializer (:game-state-from-welcome welcome-state (make-game-state))
  :uint8   game-state-board-size
  :uint8   game-state-ships
  :float32 game-state-energy
  :uint16  game-state-missiles
  :string  game-state-opponent)

(defun handle-message-from-server (message)
  (userial:with-buffer message
    (ecase (userial:unserialize :server-opcode)
      (:welcome      (handle-welcome-message message))
      (:ack          (handle-ack-message message))
      (:sunk         (handle-sunk-message message))
      (:shot-results (handle-shot-results-message message)))))

(defun handle-welcome-message (message)
  (format t "server started match with opponent: ")
  (finish-output)
  (userial:with-buffer message
    (userial:unserialize :game-state-from-welcome :welcome-state *game-state*)
    (setf (game-state-current-screen *game-state*) :place-ships)
    (format t "~a~%" (game-state-opponent *game-state*))
    (finish-output)))

(defun make-login-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode      :login
			:string               name)
    (userial:get-buffer)))

(defun make-place-ship-message (x y orientation)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :place-ship
			:float32 x
			:float32 y
			:orientation orientation)))

(defun make-ping-message (x y radius)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :ping
			:float32       radius
			:float32       x
			:float32       y)))
