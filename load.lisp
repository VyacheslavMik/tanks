(push (directory-namestring *load-truename*) asdf:*central-registry*)
(asdf:load-system "tanks")
(setf (symbol-value (find-symbol "*BASE-DIR*" (find-package "TANKS")))
      (directory-namestring *load-truename*))
