;;;; battleship.asd

(asdf:defsystem #:battleship
  :serial t
  :description "A modern OpenGL networked battleship game"
  :author "Ezra Rush <rushwest@gmailcom>"
  :license "The MIT License (MIT) Copyright (c) 2014 Ezra Rush"
  :depends-on (#:sb-cga
	       #:cl-opengl
	       #:sdl2
	       #:usocket
               #:userial)
  :components ((:file "package")
	       (:module "graphics"
			:components ((:file "utils")
				     (:file "camera")
				     (:file "pipeline")
				     (:file "technique")
				     (:file "color-technique")
				     (:file "primative")))
	       (:module "network"
			:components ((:file "common")
				     (:file "server")
				     (:file "client")))	       
	       (:file "picking")
	       (:file "ship")
	       (:file "ping")
	       (:file "missile")
	       (:file "graphics-engine")
               (:file "battleship")))

