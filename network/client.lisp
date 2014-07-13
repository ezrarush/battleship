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
    (ecase (userial:unserialize :server-opcodes)
      (:welcome      (handle-welcome-message message))
      (:ack          (handle-ack-message message))
      (:sunk         (handle-sunk-message message))
      (:shot-results (handle-shot-results-message message)))))

(defun handle-welcome-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:game-state-from-welcome game-state)
      ;; do anything with this game state here
      )))

(defun make-login-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcodes      :login
			:string               name)
    (userial:get-buffer)))

(defun make-place-ship-message (x y orientation)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :place-ship
			:int8 x
			:int8 y
			:orientation orientation)))

