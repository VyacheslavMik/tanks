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
   (moving-p :accessor moving-p :initarg :moving-p)))

(defun make-tank (hull tracks weapon)
  (let ((tracks (make-instance 'animation :textures tracks)))
    (stop-animation tracks)
    (make-instance 'tank
		   :hull hull
		   :tracks tracks
		   :weapon weapon)))

(defun update-tank (tank frame-time)
  (if (key-pressed-p :w)
      (play-animation (tracks tank))
      (stop-animation (tracks tank)))
  (update-animation (tracks tank) frame-time))

(defun draw-animation (animation cx cy dx dy angle)
  (with-slots (textures current-frame) animation
    (let ((texture (aref textures current-frame)))
      (al:draw-rotated-bitmap texture cx cy dx dy angle 0))))

(defparameter *angle* 0)

(defun draw-tank (tank x y)
  (let ((cx 128)
	(cy 128)
	(angle (/ (* *angle* pi) 180)))
    (with-slots (hull tracks weapon) tank
      (al:draw-rotated-bitmap hull cx cy x y angle 0)
      (draw-animation tracks 76 120 x y angle)
      (draw-animation tracks -34 120 x y angle)
      (al:draw-rotated-bitmap (base weapon) 47 15 x y angle 0)
      (al:draw-rotated-bitmap (barrel weapon) 18 132 x y angle 0))))

(defparameter *tank* nil)

(defmethod al:update ((sys game))
  (when (key-pressed-p :r)
    (incf *angle* 1))
  (when (key-pressed-p :e)
    (setf *angle* 0))
  (let ((frame-time (al:frame-time sys)))
    (update-tank *tank* frame-time)))

(defmethod al:render ((sys game))
  (al:clear-to-color (al:map-rgb 128 128 128))
  (draw-tank *tank* 200 200)
  ;;  (al:draw-scaled-bitmap (aref *tank-hulls* 0) 0 0 256 256 200 200 64 64 0)
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
			  (aref *tank-weapons* 0))))

(cffi:defcallback %%al-main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (al:run-system (make-instance 'game))
  0)

(defun %al-main ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %%al-main))))

(defun al-main ()
  (sb-thread:interrupt-thread (sb-thread:main-thread) #'%al-main))
