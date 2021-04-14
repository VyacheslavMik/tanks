(in-package #:tanks)

(defparameter *base-dir* nil)

(defun make-file-path (path)
  (concatenate 'string *base-dir* path))

(defun load-bitmap (path)
  (al:load-bitmap (make-file-path path)))

(defclass game (al:system)
  ()
  (:default-initargs
   :width 1024 :height 768
   :title "Tanks"
   :logic-fps 60
   :display-flags '(:opengl :opengl-3-0)
   :display-options '((:sample-buffers 1 :suggest)
        	      (:samples 8 :suggest))))

(defclass tank-weapon ()
  ((base :accessor base :initarg :base)
   (barrel :accessor barrel :initarg :barrel)))

(defclass tank-texture ()
  ((hull :accessor hull)
   (tracks :accessor tracks)))

(defparameter *tank-weapons* (make-array '(8)))
(defparameter *tank-hulls* (make-array '(8)))
(defparameter *tank-tracks* (make-array '(4)))

(defun load-tank-weapon (idx)
  (let* ((fmt "assets/Weapon_Color_A/Gun_0~a_~a.png")
	 (barrel (format nil fmt idx "A"))
	 (base (format nil fmt idx "B")))
    (make-instance 'tank-weapon
		   :barrel (load-bitmap barrel)
		   :base (load-bitmap base))))

(defun load-tank-weapons ()
  (loop for idx from 1 to 8
	do (setf (aref *tank-weapons* (1- idx)) (load-tank-weapon idx))))

(defun load-tank-hull (idx)
  (let ((fmt "assets/Hulls_Color_A/Hull_0~a.png"))
    (load-bitmap (format nil fmt idx))))

(defun load-tank-hulls ()
  (loop for idx from 1 to 8
	do (setf (aref *tank-hulls* (1- idx)) (load-tank-hull idx))))

(defun load-tank-track (idx)
  (let ((fmt "assets/Tracks/Track_~a_~a.png"))
    (vector
     (load-bitmap (format nil fmt idx "A"))
     (load-bitmap (format nil fmt idx "B")))))

(defun load-tank-tracks ()
  (loop for idx from 1 to 4
	do (setf (aref *tank-tracks* (1- idx)) (load-tank-track idx))))

(defparameter *pressed-keys* ())

(defun key-pressed-p (key)
  (member key *pressed-keys*))

(defclass animation ()
  ((textures :accessor textures :initarg :textures)
   (frame-timer :accessor frame-timer :initform 0.0)
   (frame-delay :accessor frame-delay :initform 0.05)
   (current-frame :accessor current-frame :initform 0)
   (loop-animation :accessor loop-animation :initform t)
   (finished-playing :accessor finished-playing :initform nil)))

(defun play-animation (animation)
  (when (finished-playing animation)
    (setf (current-frame animation) 0)
    (setf (finished-playing animation) nil)))

(defun stop-animation (animation)
  (unless (finished-playing animation)
    (setf (current-frame animation) 0)
    (setf (finished-playing animation) t)))

(defun update-animation (animation frame-time)
  (unless (finished-playing animation)
    (incf (frame-timer animation) frame-time)
    (when (> (frame-timer animation) (frame-delay animation))
      (incf (current-frame animation))

      (when (>= (current-frame animation) (length (textures animation)))
	(if (loop-animation animation)
	    (setf (current-frame animation) 0)
	    (progn
	      (setf (current-frame animation) (1- (length (textures animation))))
	      (setf (finished-playing animation) t))))

      (setf (frame-timer animation) 0.0))))

(defclass tank ()
  ((hull :accessor hull :initarg :hull)
   (tracks :accessor tracks :initarg :tracks)
   (weapon :accessor weapon :initarg :weapon)
   (moving-p :accessor moving-p :initarg :moving-p)
   (moving-direction :accessor moving-direction :initform :up)
   (pos-x :accessor pos-x :initarg :pos-x :initform (error "pos-x must be provided"))
   (pos-y :accessor pos-y :initarg :pos-y :initform (error "pos-y must be provided"))
   (velocity :accessor velocity :initform 200)
   (turn-rate :accessor turn-rate :initform 90)
   (current-angle :accessor current-angle :initform 0)))

(defparameter *font* nil)

(defun draw-text (text x y)
  (al:draw-text *font* (al:map-rgba-f 1 1 1 0) x y 0 text))

(defun make-tank (hull tracks weapon x y)
  (let ((tracks (make-instance 'animation :textures tracks)))
    (stop-animation tracks)
    (make-instance 'tank
		   :hull hull
		   :tracks tracks
		   :weapon weapon
		   :pos-x x
		   :pos-y y)))

(defun get-angle-for-direction (direction)
  (ecase direction
    (:up 0)
    (:right 90)
    (:down 180)
    (:left 270)))

(defparameter *debug-p* t)

(defun print-message (fmt &rest args)
  (when *debug-p*
    (setf *debug-p* nil)
    (apply #'format t fmt args)))

;; TODO: refactor function
(defun update-tank-angle (tank frame-time)
  (with-slots (moving-direction current-angle moving-p) tank
    (when moving-p
      (let ((desired-angle (get-angle-for-direction moving-direction)))
	(if (< (abs (- desired-angle current-angle)) 1)
	    (setf current-angle desired-angle)
	    (let ((rotate-clockwise-p (< (mod (- desired-angle current-angle) 360) 180))
		  (angle (* (turn-rate tank) frame-time)))
	      (if rotate-clockwise-p
		  (incf (current-angle tank) angle)
		  (decf (current-angle tank) angle))
	      (cond
		((> (current-angle tank) 360)
		 (decf (current-angle tank) 360))

		((< (current-angle tank) 0)
		 (incf (current-angle tank) 360)))))))))

(defun tank-turning-p (tank)
  (with-slots (moving-direction current-angle moving-p) tank
    (and moving-p
	 (/= (get-angle-for-direction moving-direction)
	     current-angle))))

(defun update-tank-position (tank frame-time)
  (with-slots (moving-direction moving-p pos-x pos-y velocity) tank
    (when (and moving-p (not (tank-turning-p tank)))
      (let ((delta (* velocity frame-time)))
	(ecase moving-direction
	  (:up (decf pos-y delta))
	  (:down (incf pos-y delta))
	  (:left (decf pos-y delta))
	  (:right (incf pos-x delta)))))))

(defun update-tank (tank frame-time)
  (if (moving-p tank)
      (play-animation (tracks tank))
      (stop-animation (tracks tank)))
  (update-animation (tracks tank) frame-time)
  (update-tank-angle tank frame-time)
  (update-tank-position tank frame-time))

(defun draw-animation (animation cx cy dx dy angle)
  (with-slots (textures current-frame) animation
    (let ((texture (aref textures current-frame)))
      (al:draw-rotated-bitmap texture cx cy dx dy angle 0))))

(defun draw-tank (tank)
  (with-slots (hull tracks weapon current-angle pos-x pos-y) tank
    (let ((angle (/ (* current-angle pi) 180)))
      (al:draw-rotated-bitmap hull 128 128 pos-x pos-y angle 0)
      (draw-animation tracks 76 120 pos-x pos-y angle)
      (draw-animation tracks -34 120 pos-x pos-y angle)
      (al:draw-rotated-bitmap (base weapon) 47 15 pos-x pos-y angle 0)
      (al:draw-rotated-bitmap (barrel weapon) 18 132 pos-x pos-y angle 0))))

(defparameter *tank* nil)

(defmethod al:update ((sys game))
  (cond
    ((key-pressed-p :w)
     (setf (moving-direction *tank*) :up)
     (setf (moving-p *tank*) t))

    ((key-pressed-p :d)
     (setf (moving-direction *tank*) :right)
     (setf (moving-p *tank*) t))

    ((key-pressed-p :s)
     (setf (moving-direction *tank*) :down)
     (setf (moving-p *tank*) t))

    ((key-pressed-p :a)
     (setf (moving-direction *tank*) :left)
     (setf (moving-p *tank*) t))

    (t
     (setf (moving-p *tank*) nil)))
				   
  (let ((frame-time (al:frame-time sys)))
    (update-tank *tank* frame-time)))

(defmethod al:render ((sys game))
  (al:clear-to-color (al:map-rgb 128 128 128))
  (draw-tank *tank*)
;;  (draw-text (format nil "Direction: ~a" (moving-direction *tank*)) 500 10)
;;  (draw-text (format nil "Angle: ~a" (current-angle *tank*)) 500 40)
  (al:flip-display))

(defmethod al:key-down-handler ((sys game))
  (let ((key (cffi:foreign-slot-value (al:event sys)
				      '(:struct al:keyboard-event)
				      'al::keycode)))
    (pushnew key *pressed-keys*)))

(defmethod al:key-up-handler ((sys game))
  (let ((key (cffi:foreign-slot-value (al:event sys)
				      '(:struct al:keyboard-event)
				      'al::keycode)))
    (setf *pressed-keys* (remove key *pressed-keys*))))

(defmethod al:system-loop :before ((sys game))
  (declare (ignore sys))
  (load-tank-weapons)
  (load-tank-tracks)
  (load-tank-hulls)
  (setf *tank* (make-tank (aref *tank-hulls* 0)
			  (aref *tank-tracks* 0)
			  (aref *tank-weapons* 0)
			  200
			  200))
  (setf *font* (al:load-ttf-font "/System/Library/Fonts/Supplemental/Arial.ttf" 32 0)))

(cffi:defcallback %%al-main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (al:run-system (make-instance 'game))
  0)

(defun %al-main ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %%al-main))))

(defun al-main ()
  (sb-thread:interrupt-thread (sb-thread:main-thread) #'%al-main))
