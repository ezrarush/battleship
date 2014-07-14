(in-package #:battleship)

(defvar *server* t)

(defvar *server-socket* nil)

(defun server-p () *server*)

(defvar *clients* nil)

(defun start-server (server-ip port)
  (assert (not *server-socket*))
  (setf *server-socket*
	(usocket:socket-listen server-ip
			       port
			       :reuseaddress t
			       :element-type '(unsigned-byte 8))))

(defun stop-server ()
  (assert *server-socket*)
  (usocket:socket-close *server-socket*)
  (setf *server-socket* nil
	*clients* nil))

(defun accept-client ()
  (when (usocket:wait-for-input *server-socket*
				:timeout 0
				:ready-only t)
    (push (usocket:socket-accept *server-socket*) *clients*)))

(defun handle-message-from-client (message)
  (finish-output)
  (userial:with-buffer message
    (ecase (userial:unserialize :client-opcodes)
      (:login      (handle-login-message message))
      (:place-ship (handle-place-ship-message message))
      (:ping       (handle-ping-message message))
      (:fire       (handle-ping-message message)))))

(defun handle-login-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
			      (assert (plusp (length name)))
			      (format t "~a has joined the server~%" name)
			      (match-or-queue name))))

(defun match-or-queue (name)
  (push (make-instance 'player :name name :ships 5 :energy 10.0 :missiles 20 :opponent "XXX") *players*)
  (when (eql (length *players*) 2)
    (format t "2 players logged in: starting match.~%")
    (finish-output)
    (loop for client in *clients* do
	 (send-message client (make-welcome-message 40 5 10.0 20 "XXX")))))

(defun handle-place-ship-message (message)
   (let (x y orientation)
     (userial:with-buffer message
       (userial:unserialize* :int8        x
			     :int8        y
			     :orientation orientation))
     (add-ship-to-map x y
		      :is-vertical (eql orientation
					:vertical))))

(defun make-welcome-message (squares ships energy missiles opponent)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcodes :welcome
			:uint8 squares
			:uint8 ships
			:float32 energy
			:uint16 missiles
			:string opponent)))
