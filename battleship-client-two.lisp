(ql:quickload "battleship")
;(ql:quickload "swank")
;(bt:make-thread (lambda () (swank:create-server :port 4007 :dont-close t)))
(battleship:main :server-p nil :server-ip "127.0.0.1" :name "Akane Tendo")
