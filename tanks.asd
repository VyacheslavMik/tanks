(in-package #:asdf)

(defsystem #:tanks
  :version "0.1"
  :description "2D tanks"
  :depends-on (#:sdl2
               #:sdl2-image)
  :components ((:module "game"
		:components ((:file "package")
			     (:file "core")))))
