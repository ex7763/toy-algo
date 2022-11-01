;;;; visibility-polygon.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
     ;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3)
(ql:quickload :array-operations)
(ql:quickload :cl-glfw3-examples)
(ql:quickload :iterate)
(defpackage :visibility-polygon
  (:use :cl :alexandria :trivial-main-thread :iterate)
  (:export :main))
(in-package :visibility-polygon)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

(defparameter *item-list* '())


(defun draw-line (x0 y0 x1 y1)
  (gl:with-primitive :lines
    (gl:vertex x0 y0 0.0)
    (gl:vertex x1 y1 0.0)
    ))

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

(defmethod polygon-edges ((self polygon))
  (let* ((points (polygon-points self))
         (px (aref (polygon-center self) 0)) 
         (py (aref (polygon-center self) 1))
         (center-point (make-array '(2) :initial-contents (list px py)))
         (edge-list '()))
    (dotimes (i (- (length points) 1))
      (pushnew 
        (list (aops:each #'+ center-point (nth i points))
              (aops:each #'+ center-point (nth (+ i 1) points)))
        edge-list))
    edge-list) 
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
(polygon-edges *poly1*)

(defun cross (v0 v1)
  (let ((x0 (aref v0 0))
        (y0 (aref v0 1))
        (x1 (aref v1 0))
        (y1 (aref v1 1))
        )
    (- (* x0 y1) (* x1 y0))
    ) 
  )

(cross #(10 10) #(10 10))

(defun cross-3p (o a b)
  (let ((o-x (aref o 0))
        (o-y (aref o 1))
        (a-x (aref a 0))
        (a-y (aref a 1))
        (b-x (aref b 0))
        (b-y (aref b 1))
        )
    (- (* (- a-x o-x) (- b-y o-y))
       (* (- a-y o-y) (- b-x o-x))))
  )

(defun intersection-1d (a0 a1 b0 b1)
  ;(declare (type double-float a0 a1 b0 b1))
  ;(declare (type single-float a0 a1 b0 b1))
  (when (> a0 a1)
    (rotatef a0 a1))
  (when (> b0 b1)
    (rotatef b0 b1))
  (<= (max a0 b0)
      (min a1 b1))
  )
(intersection-1d 1.0 2.0 3.0 1.0)

(defun intersectionp (a0 a1 b0 b1)
  (let ((a0-x (aref a0 0))
        (a0-y (aref a0 1))
        (a1-x (aref a1 0))
        (a1-y (aref a1 1))
        (b0-x (aref b0 0))
        (b0-y (aref b0 1))
        (b1-x (aref b1 0))
        (b1-y (aref b1 1))
        )
    (if (and (intersection-1d a0-x a1-x b0-x b1-x)
         (intersection-1d a0-y a1-y b0-y b1-y)
         (<=  (* (cross-3p a0 a1 b0) (cross-3p a0 a1 b1)) 0)
         (<=  (* (cross-3p b0 b1 a0) (cross-3p b0 b1 a1)) 0)
         )
        1
        0)
    ))

(intersectionp #(0.0 0.0) #(1.0 1.0)
               #(1.0 0.0) #(0.0 1.0))
(intersectionp #(0.0 0.0) #(1.0 1.0)
               #(0.0 0.0) #(1.0 1.0))
(intersectionp #(0.0 0.0) #(1.0 1.0)
               #(1.0 1.0) #(1.0 2.0))
(intersectionp #(0.0 0.0) #(1.0 1.0)
               #(2.0 2.0) #(1.0 2.0))
(polygon-edges *poly1*)

(defun draw-mouse-to-all-point (x y polygon)
  (let* ((px (aref (polygon-center polygon) 0)) 
         (py (aref (polygon-center polygon) 1))
         (line-to-vertices 
           (mapcar (lambda (p)
                     ;(draw-line x y (+ px (aref p 0)) (+ py (aref p 1)))
                     ;(apply #'draw-line  (list x y (+ px (aref p 0)) (+ py (aref p 1))))
                     (list x y (+ px (aref p 0)) (+ py (aref p 1)))
                     )
                   (polygon-points polygon)) 
           ))
    (setf line-to-vertices
          (remove-if
            (lambda (line)
              (if (< 2 (reduce #'+
                               (mapcar
                                 (lambda (edge) 
                                   (intersectionp 
                                     (make-array '(2)
                                                 :initial-contents (list (nth 0 line)
                                                                         (nth 1 line)))
                                     (make-array '(2)
                                                 :initial-contents (list (nth 2 line)
                                                                         (nth 3 line)))
                                     (nth 0 edge)
                                     (nth 1 edge)
                                     ) 
                                   )
                                 (polygon-edges polygon))))
                  t
                  nil)
              )
                     line-to-vertices
                     ))
    (mapcar (lambda (line)
              (apply #'draw-line line))
            line-to-vertices)
    )
  )
(nth 0 '(1 2))
(nth 0 (nth 0 (polygon-edges *poly1*)))
(reduce (lambda (a b) (or a b)) '(t nil t nil))
(reduce (lambda (a b) (or a b)) '(nil nil nil nil))

(let ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    ;; drag polygon
    (when *buttons-pressed*
      (multiple-value-bind (x y) 
          (values-list (get-cursor-position))

        (draw-mouse-to-all-point x y *poly1*)
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
    (with-init-window (:title "Visibility Polygon" :width +screen-width+ :height +screen-height+)
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
