;;;; boids.lisp
;;;; Author: Bo-Jiun Hsu
;;;; Reference: https://vergenet.net/~conrad/boids/pseudocode.html


(ql:quickload :cl-glfw3)
(ql:quickload :cl-glfw3-examples)
(ql:quickload :magicl)
(ql:quickload :array-operations)

(defpackage :boids
  (:use :cl :cl-user :glfw :alexandria :trivial-main-thread)
  (:import-from :magicl
                :rand
                :tref
                :from-list
                :zeros
                :.+
                :.-
                :.*
                :./
                )
  (:export :main))
(in-package :boids)

(defconstant +screen-width+ 800)
(defconstant +screen-height+ 800)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

(defparameter *boids-list* '())

(defparameter *boids-velocity-limit* 5.0)

(defun draw-line (x0 y0 x1 y1)
  (gl:with-primitive :lines
    (gl:vertex x0 y0 0.0)
    (gl:vertex x1 y1 0.0)
    ))

(defun draw-vec (start end)
  (gl:with-primitive :lines
    (gl:vertex (tref start 0) (tref start 1) 0.0)
    (gl:vertex (tref end 0) (tref end 1) 0.0)
    ))

(defun draw-circle (vec r &optional (num-points 16))
  (let ((x0 (magicl:tref vec 0)) 
        (y0 (magicl:tref vec 1)))
    (gl:with-primitive :line-loop
      (let ((angle (/ (* 2 pi) num-points)))
        (dotimes (i num-points)
          (gl:vertex (+ x0 (* r (sin (* i angle))))
                     (+ y0 (* r (cos (* i angle))))
                     0.0))
        ))))

(defun pos-x (vec)
  (tref vec 0))

(defun pos-y (vec)
  (tref vec 1))

(defun draw-boids (boids)
  (let* ((angle 0)
         (pos (pos boids))
         (vel (vel boids))
         (pos+vel (.+ pos (.* 15.0 vel))))
    (draw-vec pos pos+vel)
    (draw-circle pos 8)))


(defclass boids ()
  ((vel
     :initform (.- 0.5 (rand '(2) :type 'single-float))
     :accessor vel)
   (pos 
     :initform (from-list (list (coerce (random +screen-width+) 'single-float) 
                                (coerce (random +screen-height+) 'single-float))
                          '(2)
                          :type 'single-float)
     :accessor pos)))

(defun create-boids-list (num)
  (if (< num 1)
      nil
      (append (create-boids-list (- num 1))
              (list (make-instance 'boids)))))
(create-boids-list 5)

;;;; Three Rule
;;;; Separation: steer to avoid crowding local flockmates
;;;; Alignment: steer towards the average heading of local flockmates
;;;; Cohesion: steer to move towards the average position (center of mass) of local flockmates
(defun separation-rule (boids boids-list)
  ;;; TODO
  (.-
    (reduce #'.+
            (mapcar #'(lambda (other-boids)
                        (if (< (magicl:norm (.- (pos boids) 
                                                (pos other-boids)))
                               200)
                            (.- (pos boids) (pos other-boids))
                            (zeros '(2) :type 'single-float)))
                    boids-list))
    (zeros '(2) :type 'single-float)) 
  )

(separation-rule (make-instance 'boids) (create-boids-list 5))

(defun alignment-rule (boids boids-list)
  (./ 
    (.+ 
      (./ (reduce #'.+ 
                  (mapcar #'(lambda (other-boids)
                              (vel other-boids)
                              )
                          boids-list))
          (length boids-list)) 
      (vel boids)
      )
    8.0)
  )

(defun cohesion-rule (boids COM)
  (./ (.- COM (pos boids))
    100))

(defun limit-velocity (boids)
  (let ((norm (magicl:norm (vel boids)))
        (norm-vec (magicl:normalize (vel boids))))
    (setf (vel boids)
          (.* norm-vec (min norm *boids-velocity-limit*)))
    )
  )

(defun center-of-mass (boids-list)
  (./ (reduce #'.+ (mapcar #'(lambda (boids)
                               (pos boids))
                           boids-list))
      (length boids-list)))

(defun update-boids (boids boids-list COM)
  (let* ((alignment (alignment-rule boids boids-list))
         (cohesion (cohesion-rule boids COM))
         (separation (.* 0.1 (separation-rule boids boids-list)))

         (alignment (zeros '(2) :type 'single-float))
         (cohesion (zeros '(2) :type 'single-float))
         ;; (separation (zeros '(2) :type 'single-float))
         (all (.+ alignment cohesion separation))
    )

    (setf (vel boids) (.+ (vel boids) all))
    (limit-velocity boids)
    (setf (pos boids) (magicl:map #'(lambda (pos)
                                      (mod pos +screen-width+))
                                  (.+ (pos boids) (vel boids))))))

(defun draw-boids-list (boids-list)
  (mapcar #'(lambda (boids)
              (draw-boids boids))
          boids-list))

(defun update-boids-list (boids-list)
  (let ((COM (center-of-mass boids-list)))
    (mapcar #'(lambda (boids)
                (update-boids boids boids-list COM))
            boids-list)) 
  )

; (center-of-mass (create-boids-list 10))

(let* ((click nil)
       (boids-list (create-boids-list 20)))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    (update-boids-list boids-list)
    (draw-boids-list boids-list)
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

  ;(gl:ortho (- +screen-width+) +screen-width+ +screen-height+ (- +screen-height+) 0 1)

  ;; (gl:ortho 0 (/ +screen-width+ 2) (/ +screen-height+ 2)
  ;;           (- (/ +screen-width+ 2)) (- (/ +screen-height+ 2))  1)

  ;(gl:matrix-mode :modelview)
  ;(gl:load-identity)
  )

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun main ()
  ;; Graphics calls on OS X must occur in the main thread
  (with-body-in-main-thread ()
    (with-init-window (:title "boids" :width +screen-width+ :height +screen-height+
                       ;;; Enable higher OpenGL version
                       ;; :context-version-major 3
                       ;; :context-version-minor 2
                       ;; :opengl-profile :opengl-core-profile
                       ;; :opengl-forward-compat t
                       ;;; ex7763/cl-glfw3
                       ;; :scale-to-monitor t
                       ;; :cocoa-retina-framebuffer nil
                       ;; :cocoa-graphics-switching nil
                       )
      ; (%glfw:swap-interval 1)  ; Disable Vsync (0)
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
; (funcall (find-symbol "QUIT" "COMMON-LISP-USER"))
