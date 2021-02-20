(in-package #:tanks)

(defun run-game ()
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:video)
       (sdl2:with-window (win :flags '(:shown :opengl))
	 (sdl2:with-gl-context (gl-context win)
	   (sdl2:with-event-loop (:method :poll)
	     (:keydown (:keysym keysym)
		       (let ((scancode (sdl2:scancode-value keysym))
			     (sym (sdl2:sym-value keysym))
			     (mod-value (sdl2:mod-value keysym)))
			 (cond
	                   ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
	                   ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
	                   ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
			 (format t "Key sym: ~a, code: ~a, mod: ~a~%"
				 sym
				 scancode
				 mod-value)))

	     (:keyup (:keysym keysym)
		     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		       (sdl2:push-event :quit)))

	     (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
	                   (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
	                           x xrel y yrel state))

	     (:idle ()
                    (gl:clear :color-buffer)
                    (gl:begin :triangles)
                    (gl:color 1.0 0.0 0.0)
                    (gl:vertex 0.0 1.0)
                    (gl:vertex -1.0 -1.0)
                    (gl:vertex 1.0 -1.0)
                    (gl:end)
                    (gl:flush)
                    (sdl2:gl-swap-window win))

	     (:quit () t))))))))

(defun run-on-mac-in-slime ()
  (sb-thread:interrupt-thread (sb-thread:main-thread) #'run-game))
