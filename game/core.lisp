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

(defmethod al:update ((sys game))
  (declare (ignore sys))
  ;; do some update logic here
  )

(defmethod al:render ((sys game))
  (al:clear-to-color (al:map-rgb 128 128 128))
  (let* ((time (al:system-time sys))
	 (track-idx (if (zerop (mod (floor (* time 1000)) 16))
			0
			1)))
    (al:draw-bitmap (aref *tank-hulls* 0) 100 100 0)
    ;;    (print al:system-time)
    (al:draw-bitmap (aref (aref *tank-tracks* 0) track-idx) 152 110 0)
    (al:draw-bitmap (aref (aref *tank-tracks* 0) track-idx) 262 110 0)
    (al:draw-bitmap (base (aref *tank-weapons* 0)) 180 200 0)
    (al:draw-bitmap (barrel (aref *tank-weapons* 0)) 210 90 0))
  ;;  (al:draw-scaled-bitmap (aref *tank-hulls* 0) 0 0 256 256 200 200 64 64 0)
  (al:flip-display))

(defmethod al:system-loop :before ((sys game))
  (declare (ignore sys))
  (load-tank-weapons)
  (load-tank-tracks)
  (load-tank-hulls))

(cffi:defcallback %%al-main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (al:run-system (make-instance 'game))
  0)

(defun %al-main ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (al:run-main 0 (cffi:null-pointer) (cffi:callback %%al-main))))

(defun al-main ()
  (sb-thread:interrupt-thread (sb-thread:main-thread) #'%al-main))
