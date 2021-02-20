(in-package #:asdf)

(defsystem #:tanks
  :version "0.1"
  :description "2D tanks"
  :components
  ((:module "game"
    :components ((:file "package")
		 (:file "core")))))
