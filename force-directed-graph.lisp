;;;; force-directed-graph.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3)
(ql:quickload :cl-glfw3-examples)
(ql:quickload :zpb-ttf)
(ql:quickload :cl-glu)
(ql:quickload :magicl)
;(in-package :cl-glfw3-examples)
(defpackage :force-directed-graph
  ;(:use :cl :cl-user :cl-glfw3 :cl-glfw3-examples)
  (:use :cl :cl-user :glfw :cl-glu :alexandria :trivial-main-thread)
  (:export :main))
(in-package :force-directed-graph)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun draw-line (x0 y0 x1 y1)
  (gl:with-primitive :lines
    (gl:vertex x0 y0 0.0)
    (gl:vertex x1 y1 0.0)
  ))

(defun draw-circle (vec r &optional (num-points 16))
  (gl:line-width 1)
  (let ((x0 (magicl:tref vec 0)) 
        (y0 (magicl:tref vec 1)))
    (gl:with-primitive :line-loop
      (let ((angle (/ (* 2 pi) num-points)))
        (dotimes (i num-points)
          (gl:vertex (+ x0 (* r (sin (* i angle))))
                     (+ y0 (* r (cos (* i angle))))
                     0.0))
        ))))

(defstruct vertex
  pos  ; array
  disp
  (value (+ 10 (random 5)))
  )

(defstruct edge
  v  ; vertex 1
  u  ; vertex 2
  w  ; weight
  )

(defun draw-vertex (v)
  (draw-circle (magicl:.* (vertex-pos v)
                          (magicl:from-list (list +screen-width+ +screen-height+) '(2)))
               (vertex-value v)))

(defun draw-edge (e)
  (let* ((v (edge-v e))
         (u (edge-u e))
         (pos-v (magicl:.* (vertex-pos v)
                           (magicl:from-list (list +screen-width+ +screen-height+) '(2)) 
                           ))
         (pos-u (magicl:.* (vertex-pos u)
                           (magicl:from-list (list +screen-width+ +screen-height+) '(2)) 
                           ))
         (x0 (magicl:tref pos-v 0))
         (y0 (magicl:tref pos-v 1))
         (x1 (magicl:tref pos-u 0))
         (y1 (magicl:tref pos-u 1))
         )
    (gl:line-width (/ (edge-w e) 2))
    (draw-line x0 y0 x1 y1)))

(defparameter *num-node* 7)
(defparameter *vertex-list* (list (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2))) 
                                  (make-vertex :pos (magicl:rand '(2)))))
(defparameter *edge-list* (mapcar (lambda (_)
                                          (make-edge :v (nth (random *num-node*) *vertex-list*)
                                                     :u (nth (random *num-node*) *vertex-list*)
                                                     :w (random 10)))
                                  (range 10)))
(write *edge-list*)

(defun force-directed-graph ()
  (let ((temperature 10))
    (dotimes (i 100)
      (dolist (v *vertex-list*)
        (setf (vertex-disp v) 0)
        (dolist (u *vertex-list*)
          (let ((d (magicl:.- (vertex-pos v) (vertex-pos u))))
            (setf (vertex-disp v) ))))
      )))

(let* ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    ;; draw vertex and edge
    (dolist (e *edge-list*)
      (gl:color 1.0 0.0 0.0)
      (draw-vertex (edge-v e))
      (draw-vertex (edge-u e)) 

      (gl:color 0.3 0.5 0.0)
      (draw-edge e)
      )
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
    (with-init-window (:title "force-directed-graph" :width +screen-width+ :height +screen-height+
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
