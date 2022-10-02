;;;; separating-axis-theorem.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3)
(ql:quickload :cl-glfw3-examples)
(in-package :cl-glfw3-examples)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)
(defconstant +quadtree-capacity+ 4)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

(defparameter *item-list* '())

(defun draw-rect (x0 y0 x1 y1)
  (gl:with-primitive :line-loop
    (gl:vertex x0 y0 0.0)
    (gl:vertex x0 y1 0.0)
    (gl:vertex x1 y1 0.0)
    (gl:vertex x1 y0 0.0)
    )
  )

(defun draw-circle (x0 y0 r &optional (num-points 16))
  (gl:with-primitive :line-loop
    (let ((angle (/ (* 2 pi) num-points)))
      (dotimes (i num-points)
        (gl:vertex (+ x0 (* r (sin (* i angle))))
                   (+ y0 (* r (cos (* i angle))))
                   0.0))
      )))

(defun draw-polygon (poly)
  (let ((x (nth 0 (polygon-center poly)))  
        (y (nth 1 (polygon-center poly))))
    (gl:with-primitive :line-loop
      (mapcar (lambda (p)
                (gl:vertex (+ x (point-x p)) (+ y (point-y p)) 0.0))
              (polygon-points poly))
      )
    (draw-circle x y 15)
    )
  )

(defun draw-point (x0 y0 &optional (size 1))
  (gl:with-primitive :line-loop
    (gl:vertex (- x0 size) (- y0 size) 0.0)
    (gl:vertex (- x0 size) (+ y0 size) 0.0)
    (gl:vertex (+ x0 size) (+ y0 size) 0.0)
    (gl:vertex (+ x0 size) (- y0 size) 0.0)
    )
  )

(defstruct point
  x
  y)

(defstruct polygon
  center
  points
  (angle 0.0)
  )

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defparameter *poly1* (make-polygon 
                        ; :center '(44 36)
                        :center '(200 200)
                        :points (list (make-point :x 0 :y 0)
                                      (make-point :x 80 :y -30)
                                      (make-point :x 100 :y 100)   
                                      (make-point :x 40 :y 80)   
                                      (make-point :x 0 :y 30)
                                      )))

(let ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    (draw-circle 100 400 30)
    ;; drag polygon
    (when *buttons-pressed*
      (multiple-value-bind (x y) 
          (values-list (get-cursor-position))
        (let ((poly-x (nth 0 (polygon-center *poly1*)))  
              (poly-y (nth 1 (polygon-center *poly1*))))
          (when (and (< (abs (- poly-x x)) 15) 
                     (< (abs (- poly-y y)) 15))
            (setf (polygon-center *poly1*) (get-cursor-position))
            (gl:color 0.0 1.0 0.0)))
      ))
    (draw-polygon *poly1*)
    )
  )



;;; Create Windows
;;; Ignore this part
(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore mod-keys))
  (if (eq action :press)
      (pushnew button *buttons-pressed*)
      (deletef *buttons-pressed* button)))

; (format t "~A~%" *buttons-pressed*)
; (format t "~A~%" (get-cursor-position))

(defun set-viewport (width height)
  ;; Black background
  (gl:clear-color 0.2 0.2 0.2 0.2)

  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; 將座標從(-1, -1) (1, 1,)轉成(0, 0) (screen width, screent height)
  (gl:ortho 0 +screen-width+ +screen-height+ 0 0 1)
  ;(gl:matrix-mode :modelview)
  ;(gl:load-identity)
  )

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "Separating Axis Theorem" :width +screen-width+ :height +screen-height+)
      (set-key-callback 'quit-on-escape)
      (set-mouse-button-callback 'mouse-callback)

      ;; Callback for window resize events
      (set-window-size-callback 'update-viewport)
      (set-viewport +screen-width+ +screen-height+)

      ;; Our render-loop
      (loop until (window-should-close-p)
;            do (wait-events)
            do (render)
            do (swap-buffers)
            do (poll-events)))))

(main)
;(quit)
