;;;; quadtree.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3-examples)
(in-package :cl-glfw3-examples)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)

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

(defun draw-point (x0 y0 &optional (size 1))
  (gl:color 1.0 0.0 0.0)
  (gl:line-width 1)
  (gl:with-primitive :line-loop
    (gl:vertex (- x0 size) (- y0 size) 0.0)
    (gl:vertex (- x0 size) (+ y0 size) 0.0)
    (gl:vertex (+ x0 size) (+ y0 size) 0.0)
    (gl:vertex (+ x0 size) (- y0 size) 0.0)
    )
  )

(defstruct item
  x
  y)

(defstruct quadtree
  region  ; (x0, y0, x1, y1)
  (item-list '())
  (num-items 0)
  (nw nil)
  (ne nil)
  (sw nil)
  (se nil)
  )

(defun draw-quadtree (tree)
    (apply #'draw-rect (quadtree-region tree))
  )



(defun render ()
  (gl:clear :color-buffer)

  (gl:with-pushed-matrix
    (gl:color 0.2 0.2 1.0)
    (gl:line-width 4)
    (draw-rect 0 0 (/ +screen-width+ 2) (/ +screen-height+ 2))
    )

  (let ((root (make-quadtree :region (list 0 0 +screen-width+ +screen-height+))))
    (gl:color 1.0 0.0 0.0)
    (draw-quadtree root)
    (when *buttons-pressed*
      ; (draw-point 200 200)
      (apply #'draw-point (get-cursor-position))
      (multiple-value-bind (x y) 
          (values-list (get-cursor-position))
        (pushnew (make-item :x x :y y) *item-list*))
      )
    (mapcar (lambda (item)
              (draw-point (item-x item) (item-y item)))
            *item-list*)
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
    (with-init-window (:title "Quad Tree" :width +screen-width+ :height +screen-height+)
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
