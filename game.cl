(eval-when (:compile-toplevel :execute)
  (ql:quickload :lispbuilder-sdl))

;(declaim (optimize (speed 3) (debug 3)))

(defpackage :newcast
  (:use :common-lisp)
  (:export :run))

(in-package :newcast)

(defconstant pi/2 (coerce (/ pi 2) 'short-float) "90 degrees")
(defconstant 2pi (coerce (* 2 pi) 'short-float) "a full 360 degrees")
(defconstant -2pi (coerce (- 2pi) 'short-float) "-360 degrees")
(defconstant -pi (coerce (- pi) 'short-float) "180 degrees after conversion")
(defconstant degree (coerce (/ pi 180) 'short-float) "a single degree")
(defconstant mypi (coerce pi 'short-float))

(defparameter *width* 420 "screen width")
(defparameter *height* 300 "screen height")

(defconstant +board-size+ 40)

(defconstant +color-max+ 7)
(defun get-color (color)
  (case color
    ((0) sdl:*black*)
    ((1) sdl:*white*)
    ((2) sdl:*red*)
    ((3) sdl:*green*)
    ((4) sdl:*blue*)
    ((5) sdl:*yellow*)
    (otherwise
     sdl:*cyan*)))

(deftype board () `(simple-array fixnum (,+board-size+ ,+board-size+)))

(defun make-board (&optional fn)
  (let ((b (make-array (list +board-size+ +board-size+)
		       :element-type 'fixnum)))
    (if fn (funcall fn b))
    b))

(defun rect-of-color (x y width height color board)
  (dotimes (i width)
    (setf (aref board (+ i x) y) color
	  (aref board (+ i x) (+ y (1- height))) color))
  (dotimes (i height)
    (setf (aref board x (+ i y)) color
	  (aref board (+ x (1- width)) (+ i y)) color)))

(defparameter *game-board* ;(make-board))
  #|(make-board (lambda (board)
		(dotimes (i (* 3 +board-size+))
		  (setf (aref board 
			      (random +board-size+) 
			      (random +board-size+))
			(random +color-max+)))
		(rect-of-color 0 0 +board-size+ +board-size+ 1 board)
		(rect-of-color 10 10 20 20 3 board)
		(rect-of-color 14 14 16 16 3 board)
		(setf (aref board 12 10) 0
		      (aref board 14 15) 0))))
  |#
    (make-board (lambda (board)
		(dotimes (i 20)
		  (setf (aref board 
			      (random +board-size+) 
			      (random +board-size+))
			(random +color-max+))))))
  


(defmacro distance (x y x1 y1)
  `(sqrt (+ (expt (- ,x ,x1) 2) (expt (- ,y ,y1) 2))))

(defmacro angle-to (x y x1 y1)
  `(atan (- ,y1 ,y) (- ,x1 ,x)))

(defmacro get-intersection-coordinates (x y x-step y-step predicate)
  `(let ((nx ,x) (ny ,y))
     (loop until ,predicate
	do (incf nx ,x-step) (incf ny ,y-step)
	finally (return (values nx ny)))))

(defun outsidep (x y)
  (or (< x 0) (< y 0)
      (>= x +board-size+)
      (>= y +board-size+)))

(defun collisionp (x y map)
  (or (outsidep x y)
      (not (zerop (aref map x y)))))

(defun wall-intersection (x y x-step y-step map precision)
  precision
  (get-intersection-coordinates x y x-step y-step 
				(collisionp (round nx) (round ny) map)))

(defun wall-cast-ray (x y angle map)
  (multiple-value-bind (nx ny) 
      (wall-intersection x y (/ (cos angle) 6) (/ (sin angle) 6) map .05)
    (let ((map-nx (round nx))
	  (map-ny (round ny)))
      (values (distance x y nx ny)
	      (if (outsidep map-nx map-ny) 0 (aref map map-nx map-ny))))))

(defun draw-vertical-line-from-center (x y height color)
  (sdl:draw-vline x (- y (ash height -1)) (+ y (ash height -1))
		  :color (get-color color)))

(defun draw-vline-center (x height color)
  (draw-vertical-line-from-center x (/ *height* 2) height color))

(defmacro ray-angle (i angle fov width)
  `(+ ,angle (- (/ ,fov 2)) (* ,i (/ ,fov ,width))))

(defmacro i-from-ray-angle (ray-angle angle fov width)
  `(/ (- ,ray-angle (- ,angle (/ ,fov 2))) 
      (/ ,fov ,width)))

(defun draw-walls (x y angle fov map block-height)
  (dotimes (i *width*) ;for each vertical line on screen
    (let ((ray-angle (ray-angle i angle fov *width*)))
      (multiple-value-bind (distance color) (wall-cast-ray x y ray-angle map)
	(draw-vline-center
	 i 
	 (if (= 0 distance) block-height
	     (round (/ block-height distance)))
	 color)))))

(defstruct entity
  (x (/ +board-size+ 2.0) :type short-float)
  (y (/ +board-size+ 2.0) :type short-float)
  (speed 0.0 :type short-float)
  (direction 0.0 :type short-float)
  (fov 0.0 :type short-float)
  (update-fn (lambda (e es map) (list e es map)) :type function)
  (color 0 :type fixnum)
  (size/2 0.0 :type short-float)
  (height 0.0 :type short-float)
  (id 0 :type fixnum))

(defun angle-to-entity (x y target)
  (angle-to x y
	    (entity-x target) (entity-y target)))

(defun correct-angle-p (angle)
  (and (< angle 2pi)
       (>= angle 0)))

(defun correct-angle (angle)
  (let ((delta-angle (if (< angle 0) 2pi -2pi)))
    (loop until (correct-angle-p angle)
       do (incf angle delta-angle))
    angle))

(defun in-fov-p (fov direction angle)
  (let ((angle (correct-angle angle))
	(direction (correct-angle direction)))
    (and (<= angle (+ direction (/ fov 2)))
	 (>= angle (- direction (/ fov 2))))))

(defun single-entity-collision-p (x y e)
  (< (distance x y (entity-x e) (entity-y e))
     (entity-size/2 e)))

;if we're looking towards the entity, and it's closer than the nearest wall
(defun entity-in-view-p (x y fov direction target map)
  (let ((angle (angle-to-entity x y target)))
    (and (in-fov-p fov direction angle)
	 (>= (wall-cast-ray x y angle map)
	     (distance x y (entity-x target) (entity-y target))))))

(defun single-entity-intersection (x y x-step y-step e)
  (get-intersection-coordinates
   x y x-step y-step
   (or (outsidep nx ny)
       (single-entity-collision-p nx ny e))))

(defun cast-ray-entity (x y angle e)
  (multiple-value-bind (nx ny)
      (single-entity-intersection x y (/ (cos angle) 6) (/ (sin angle) 6) e)
    (values (distance x y nx ny)
	    (if (outsidep nx ny) ;if not an entity
		nil e))))

(defun draw-entities (x y angle fov es max-height map)
  (loop for e in es do
       (if (entity-in-view-p x y fov angle e map)
	   (let ((start-x 0))
					;(round (- (i-from-ray-angle (angle-to-entity x y e) 
					;				    angle fov *width*)
					;		  20))))
	     (dotimes (i *width*)
	       (multiple-value-bind (distance entity)
		   (cast-ray-entity x y (ray-angle (+ i start-x)
						   angle fov *width*) e)
		 (when entity
		   (draw-vline-center (+ i start-x)
				      (if (= distance 0)
					  max-height
					  (round (/ max-height distance)))
				      (entity-color entity))))))
	   (print (list x y angle fov (angle-to-entity x y e))))))


(defparameter *camera* (make-entity :speed 0.2 :fov (* 110 degree)))
(defparameter *thing* (make-entity :speed 0.00 :direction mypi
				   :color 4
				   :size/2 3.0))

(defun move-entity-lower (e map x-step y-step)
  (let ((next-x (+ (entity-x e) x-step))
	(next-y (+ (entity-y e) y-step)))
    (unless (collisionp (round next-x) (round next-y) map)
      (setf (entity-x e) next-x
	    (entity-y e) next-y))))

(defun move-entity (e map)
  (move-entity-lower e
		     map
		     (* (entity-speed e) (cos (entity-direction e)))
		     (* (entity-speed e) (sin (entity-direction e)))))

(defun update-entity (e es map)
  (funcall (entity-update-fn e) e es map)
  (move-entity e map))


(defun main-game-loop ()
  (sdl:clear-display sdl:*black*)
  (update-entity *thing* '() *game-board*)
  
  (sdl:draw-box-* 0 (/ *height* 2) *width* (/ *height* 2)
		  :color (sdl:color :r 127 :g 100 :b 100))

  (draw-walls (entity-x *camera*) (entity-y *camera*) (entity-direction *camera*)
	      (entity-fov *camera*) *game-board* *height*)

  (draw-entities (entity-x *camera*) (entity-y *camera*)
		 (entity-direction *camera*) (entity-fov *camera*)
		 (list *thing*) (/ *height* 2) *game-board*)
  
  (sdl:update-display))

(defun idle-key-handler ()
  (let ((direction (entity-direction *camera*))
	(speed (entity-speed *camera*)))  
    (when (sdl:key-down-p :sdl-key-w)
      (move-entity-lower *camera* *game-board* 
			 (* speed (cos direction)) 
			 (* speed (sin direction))))
    
    (when (sdl:key-down-p :sdl-key-s)
      (move-entity-lower *camera* *game-board*
			 (- (* speed (cos direction)))
			 (- (* speed (sin direction)))))
    
    (when (sdl:key-down-p :sdl-key-a)
      (move-entity-lower *camera* *game-board*
			 (- (* speed (cos (+ pi/2 direction))))
			 (- (* speed (sin (+ pi/2 direction))))))

    (when (sdl:key-down-p :sdl-key-d)
      (move-entity-lower *camera* *game-board*
			 (* speed (cos (+ pi/2 direction)))
			 (* speed (sin (+ pi/2 direction)))))
    
    (when (sdl:key-down-p :sdl-key-k)
      (incf (entity-direction *camera*) (* 2 degree)))
    (when (sdl:key-down-p :sdl-key-j)
      (decf (entity-direction *camera*) (* 2 degree)))))

(defun pressed-key-handler ()
  (when (sdl:key-down-p :sdl-key-escape)
    (sdl:push-quit-event))
  (when (sdl:key-down-p :sdl-key-t)
    (print (wall-cast-ray (entity-x *camera*) (entity-y *camera*)
			  (entity-direction *camera*) *game-board*))))

(defun run ()
  (sdl:with-init ()
    (sdl:window *width* *height*)
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
		       (pressed-key-handler))
      (:idle ()
	     (idle-key-handler)
	     (main-game-loop)))))
