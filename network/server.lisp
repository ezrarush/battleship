(in-package #:battleship)

(defvar *server* t)

(defvar *server-socket* nil)

(defun server-p () *server*)

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
	*players* nil))

(defun accept-client ()
  (when (usocket:wait-for-input *server-socket*
				:timeout 0
				:ready-only t)
    (make-player (usocket:socket-accept *server-socket*))))

(defun handle-message-from-client (message)
  (finish-output)
  (userial:with-buffer message
    (ecase (userial:unserialize :client-opcode)
      (:login      (handle-login-message message))
      (:place-ship (handle-place-ship-message message))
      (:ping       (handle-ping-message message))
      (:fire       (handle-ping-message message)))))

(defun handle-login-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
			      (assert (plusp (length name)))
			      (format t "~a has joined the server~%" name)
			      (finish-output)
			      (setf (name *current-player*) name)
			      (match-or-queue name))))

(defun match-or-queue (name)
  ;; this whole function and *current-player* seems too hackish
  (setf (ships *current-player*) 5)
  (setf (energy *current-player*) 10.0)
  (setf (missiles *current-player*) 20)
  (when (eql (length *players*) 2)
    (format t "2 players logged in: starting match.~%")
    (finish-output)
    
    (setf (opponent (second *players*)) (name (first *players*)))
    (setf (opponent (first *players*)) (name (second *players*)))

    (send-message (socket-connection (second *players*)) (make-welcome-message 40 5 10.0 20 (opponent (second *players*))))
    (send-message (socket-connection (first *players*)) (make-welcome-message 40 5 10.0 20 (opponent (first *players*))))))

(defun make-welcome-message (squares ships energy missiles opponent)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :welcome
			:uint8 squares
			:uint8 ships
			:float32 energy
			:uint16 missiles
			:string opponent)))

(defun handle-place-ship-message (message)
   (let (x y orientation)
     (userial:with-buffer message
       (userial:unserialize* :float32        x
			     :float32        y
			     :orientation orientation))
     (add-ship-to-map x y :is-vertical (eql orientation :vertical))
     ))

(defvar *client2-ship-list* '())
(defvar *client1-ship-list* '())

(defun add-ship-to-map (x y &key is-vertical)
  (push (list x y is-vertical) (placed-ships *current-player*)))


(defun handle-ping-message (message)
  (userial:with-buffer message
  (apply #'calculate-ping-response
         (userial:unserialize-list* '(:float32 :float32 :float32)))))

(defun calculate-ping-response (radius x y)
  (format t "ping message received from ~a~%" (name *current-player*))
  (finish-output)
  )
