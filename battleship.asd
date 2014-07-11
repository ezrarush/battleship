;;;; battleship.asd

(asdf:defsystem #:battleship
  :serial t
  :description "Describe battleship here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
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
	       (:file "missile")
	       (:file "graphics-engine")
               (:file "battleship")))

