;;;; separating-axis-theorem.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3)
(ql:quickload :array-operations)
(ql:quickload :cl-glfw3-examples)
(defpackage :sat
  (:use :cl :glfw :alexandria :trivial-main-thread)
  (:export :main))
(in-package :sat)

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
  (let ((x (aref (polygon-center poly) 0))  
        (y (aref (polygon-center poly) 1)))
    (gl:with-primitive :line-strip
      (mapcar (lambda (p)
                (gl:vertex (+ x (aref p 0)) (+ y (aref p 1)) 0.0))
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

;; simple vector library
(defun get-norm-l (v)
  (make-array 2 :initial-contents (list (- (aref v 1)) (aref v 0)))
  )
(defun get-norm-r (v)
  (make-array 2 :initial-contents (list (aref v 1) (- (aref v 0))))
  )

(defun get-length (v)
  (sqrt 
    ;; Sum all values in an array
    (aops:reduce-index #'+ i (row-major-aref 
                               (aops:each (lambda (n) (expt n 2)) v)
                               i)))
  )

(defun dot (v0 v1)
  (aops:vectorize-reduce #'+ (v0 v1) (* v0 v1)))

(dot #(3 2) #(1 2))

(get-norm-l #(1 2))
(get-norm-r #(1 2))
(get-length #(10 5))
(get-length #(10 10))

;; (defun matmatmul (mat0 mat1)
;;   (aops:each-index (i j)
;;     (aops:sum-index k
;;       (* (aref mat0 i k) (aref mat1 k j)))))
;; (matmatmul #2A((1 2) (3 4)) #2A ((2 3) (4 5)))
;; (defun matvecmul (mat vec)
;;   (aops:each-index (i j)
;;     (aops:sum-index k
;;       (* (aref mat i k) (aref vec j)))))
;; (matvecmul #2A((1 2)
;;                (3 4)) #(2 3))

(defun rotate-around-point (v point angle)
  (let ((new-v (aops:each #'- v point))
        (rotate-matrix (make-array '(2 2)
                                   :initial-contents (list (list (cos angle) (- (sin angle)))
                                                           (list (sin angle) (cos angle)))))))
  (make-array 2 :initial-contents (list (aref v 1) (- (aref v 0))))
  )

(defmethod polygon-vector-list ((self polygon))
  (let ((points (polygon-points self))
        (vector-list '()))
    (dotimes (i (- (length points) 1))
      (pushnew 
        (aops:each #'- (nth i points) (nth (+ i 1) points))
        vector-list))
    vector-list))

(defmethod polygon-points-coord ((self polygon))
  (mapcar (lambda (p)
            (aops:each #'+ (polygon-center self) p))
          (polygon-points self)
          ))

(defmethod polygon-rotate ((self polygon) degree))

(defun polygon-project-onto-axis (polygon-vector-list axis)
  "axis: #(? ?)"
  (let* ((poly-project-len-list (mapcar (lambda (v) (/ (dot (aops:each #'+ v axis) axis) (get-length axis)))
                                        polygon-vector-list)))
    poly-project-len-list  
    )
  )
   

(defun sat (poly0 poly1)
  "return t if collision"
  (let* ((poly0-vector-list (polygon-vector-list poly0)) 
         (poly1-vector-list (polygon-vector-list poly1))
         (axis-list (append (mapcar #'get-norm-l poly0-vector-list)   
                            (mapcar #'get-norm-l poly1-vector-list))))
    ;; find separating axis
      (dolist (axis axis-list)
        (let* ((poly0-project-onto-axis (polygon-project-onto-axis (polygon-points-coord poly0) axis)) 
               (poly1-project-onto-axis (polygon-project-onto-axis (polygon-points-coord poly1) axis)) 
               (poly0-min (apply #'min poly0-project-onto-axis))
               (poly0-max (apply #'max poly0-project-onto-axis))
               (poly1-min (apply #'min poly1-project-onto-axis))
               (poly1-max (apply #'max poly1-project-onto-axis))
               (result (or (> poly0-min poly1-max)
                           (> poly1-min poly0-max)))
               )
          ;; (format t "axis: ~A~%" axis)
          ;; (format t "~A ~A ~A ~A~%" poly0-min poly0-max poly1-min poly1-max)
          ;; (format t "~A~%" result)
          (when result
            (return-from sat nil))
          )
        )
    (return-from sat t)
    ;(format t "axis-list: ~A~%" axis-list)
    ))



(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))

(defparameter *poly1* (make-polygon 
                        ; :center '(44 36)
                        :center #(200 200)
                        :points (list #(0 0)
                                      #(80 -30)
                                      #(80 100)
                                      #(40 80)
                                      #(0 30)
                                      #(0 0 ))))
(defparameter *poly2* (make-polygon 
                        ; :center '(44 36)
                        :center #(300 200)
                        :points (list #(0 0)
                                      #(40 -30)
                                      #(100 -10)
                                      #(80 30)
                                      #(30 30)
                                      #(0 0))))
(polygon-vector-list *poly1*)
(sat *poly1* *poly1*)
(sat *poly1* *poly2*)

(let ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    (draw-circle 100 400 30)
    ;; drag polygon
    (when *buttons-pressed*
      (multiple-value-bind (x y) 
          (values-list (get-cursor-position))
        (let ((poly-x (aref (polygon-center *poly1*) 0))  
              (poly-y (aref (polygon-center *poly1*) 1)))
          (when (and (< (abs (- poly-x x)) 15) 
                     (< (abs (- poly-y y)) 15))
            (setf (polygon-center *poly1*) (make-array 2 
                                                       :initial-contents (get-cursor-position)))
            ;; collision detection
            (if (sat *poly1* *poly2*)
                (gl:color 0.0 0.0 1.0)  
                (gl:color 0.0 1.0 0.0)))
          )
        )
      )
    (draw-polygon *poly1*)
    (draw-polygon *poly2*)
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
