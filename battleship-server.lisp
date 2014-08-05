(ql:quickload "battleship")
(ql:quickload "swank")
(bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t)))
(battleship:main)


