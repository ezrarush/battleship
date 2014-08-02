(ql:quickload "battleship")
;(ql:quickload "swank")
;(bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
(battleship:main :server-p nil :server-ip "127.0.0.1" :name "Ranma Saotome")


