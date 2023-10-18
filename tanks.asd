(in-package #:asdf)

(defsystem #:tanks
  :version "0.1"
  :description "2D tanks"
  :depends-on (#:bordeaux-threads
               #:cffi
               #:float-features
	       #:cl-liballegro)
  :components ((:module "src"
		:components ((:file "package")
			     (:file "core"))))
  :build-operation "program-op"
  :build-pathname "tanks-sbcl"
  :entry-point "tanks:run-game")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))
