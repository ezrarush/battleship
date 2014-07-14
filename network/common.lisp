(in-package #:battleship)

(userial:make-enum-serializer :client-opcode
                      (:login :place-ship :ping :fire))

(userial:make-enum-serializer :server-opcode
                      (:welcome :ack :sunk :shot-results))

(userial:make-bitfield-serializer :playable-board-sizes
                          (:small :medium :large :huge))

(userial:make-enum-serializer :orientation 
		      (:horizontal :vertical))

(defun connected-p ()
  (or *players*
      *server-connection*))

(defun send-message (to buffer)
  (userial:with-buffer buffer
    (let ((size (userial:buffer-length))
	  (stream (usocket:socket-stream to)))
      (write-byte size stream)
      (write-sequence buffer stream :end (length buffer))
      (force-output stream))))

(defun read-message (connection)
  (let* ((buffer (userial:make-buffer))
	 (stream (usocket:socket-stream connection)))

    ;; Read the size of the message in bytes, then read those bytes
    (when (listen stream)
      (userial:with-buffer buffer
	(let* ((size (read-byte stream)))
	  (userial:buffer-advance size)
	  (read-sequence buffer stream :end size))

	(unless (zerop (userial:buffer-length))
	  (userial:buffer-rewind)
	  (if (server-p)
	      (handle-message-from-client buffer)
	      (handle-message-from-server buffer)))))))
