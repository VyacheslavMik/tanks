(in-package #:tanks)

(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor tex-width
    :initform 0 )
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
    :initform nil)))

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun set-blend-mode (tex blending)
  (sdl2:set-texture-blend-mode (tex-texture tex) blending))

(defun set-alpha (tex alpha)
  (sdl2:set-texture-alpha-mod (tex-texture tex) alpha))

(defun render (tex x y &key clip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy renderer
                      texture
                      :source-rect clip
                      :dest-rect (sdl2:make-rect x
                                                 y
                                                 (if clip (sdl2:rect-width clip) width)
                                                 (if clip (sdl2:rect-height clip) height)))))

(defun test-render-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun test-render-hello (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  ;; H
  (sdl2:render-draw-line renderer 20 20 20 100)
  (sdl2:render-draw-line renderer 20 60 60 60)
  (sdl2:render-draw-line renderer 60 20 60 100)
  ;; E
  (sdl2:render-draw-line renderer 80 20 80 100)
  (sdl2:render-draw-line renderer 80 20 120 20)
  (sdl2:render-draw-line renderer 80 60 120 60)
  (sdl2:render-draw-line renderer 80 100 120 100)
  ;; L
  (sdl2:render-draw-line renderer 140 20 140 100)
  (sdl2:render-draw-line renderer 140 100 180 100)
  ;; L
  (sdl2:render-draw-line renderer 200 20 200 100)
  (sdl2:render-draw-line renderer 200 100 240 100)
  ;; O
  (sdl2:render-draw-line renderer 260 20 260 100)
  (sdl2:render-draw-line renderer 260 100 300 100)
  (sdl2:render-draw-line renderer 300 20 300 100)
  (sdl2:render-draw-line renderer 260 20 300 20))

(defun test-render-lines (renderer)
  (sdl2:with-points ((a 200 200)
                     (b 300 400)
                     (c 400 200))
    (sdl2:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-lines renderer points num))))

(defun test-render-points (renderer)
  (sdl2:with-points ((a (random 800) (random 800))
                     (b (random 800) (random 800))
                     (c (random 800) (random 800)))
    (sdl2:set-render-draw-color renderer 0 255 0 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-points renderer points num))))

(defun test-render-rect (renderer)
  (sdl2:render-draw-rect renderer (sdl2:make-rect 400 400 35 35)))

(defun test-render-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :for y :upto 5
                   :collect (sdl2:make-rect (+ 400 (* x 10)) (+ 200 (* y 10)) 8 8)))
    (sdl2:render-draw-rects renderer rects num)))

(defun test-render-fill-rect (renderer)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 445 400 35 35)))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :collect (sdl2:make-rect (+ 500 (* x 10)) 400 8 8)))
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (sdl2:render-fill-rects renderer rects num)))

(defun run-game ()
  (sdl2:make-this-thread-main
   (lambda ()
     (sdl2:with-init (:everything)
       (sdl2:with-window (win :w 640 :h 480 :flags '(:shown))
	 (sdl2:with-renderer (renderer win :flags '(:accelerated))
           (sdl2-image:init '(:png))
	   (let ((tex (load-texture-from-file renderer "assets/Tanks/Tank01.png"))
                 (clip (sdl2:make-rect 0 0 256 256))
                 (sprite-frames 2)
                 (current-sprite-frame 0)
                 (frame 0)
                 (frames-per-sprite 8))
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
                      #+nil((gl:clear :color-buffer)
                            (gl:begin :triangles)
                            (gl:color 1.0 0.0 0.0)
                            (gl:vertex 0.0 1.0)
                            (gl:vertex -1.0 -1.0)
                            (gl:vertex 1.0 -1.0)
                            (gl:end)
                            (gl:flush))
                      (test-render-clear renderer)
                      #+nil(test-render-hello renderer)
                      #+nil(test-render-lines renderer)
                      #+nil(test-render-points renderer)
                      #+nil(test-render-rect renderer)
                      #+nil(test-render-rects renderer)
                      #+nil(test-render-fill-rect renderer)
                      #+nil(test-render-fill-rects renderer)
                      (render tex
                              (round (/ (- 640 (sdl2:rect-width clip)) 2))
                              (round (/ (- 480 (sdl2:rect-height clip)) 2))
                              :clip clip)
                      (sdl2:render-present renderer)
                      (incf frame)
                      (when (zerop (rem frame frames-per-sprite))
                        (incf current-sprite-frame)
                        (when (>= current-sprite-frame sprite-frames)
                          (setf current-sprite-frame 0))
                        (setf (sdl2:rect-x clip) (* current-sprite-frame (sdl2:rect-width clip))))
                      (sdl2:delay 25))

	       (:quit () t)))))))))

(defun run-on-mac-in-slime ()
  (sb-thread:interrupt-thread (sb-thread:main-thread) #'run-game))
