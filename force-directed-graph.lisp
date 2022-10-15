;;;; force-directed-graph.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3)
(ql:quickload :cl-glfw3-examples)
(ql:quickload :magicl)

(defpackage :force-directed-graph
  (:use :cl :cl-user :glfw :alexandria :trivial-main-thread)
  (:import-from :magicl
                :tref
                :from-list
                :zeros
                :.+
                :.- 
                :.*
                :./
  )
  (:export :main))
(in-package :force-directed-graph)

(defconstant +screen-width+ 800)
(defconstant +screen-height+ 800)

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
  ; (gl:line-width 1.0)
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
    ; (gl:line-width (/ (edge-w e) 2))
    (draw-line x0 y0 x1 y1)))

(defparameter *num-node* 40)
(defparameter *num-edges* (* *num-node* 3))
(defparameter *num-center* 3)
;; (defparameter *vertex-list* (mapcar (lambda (_)
;;                                           (make-vertex :pos (.* (.- (magicl:rand '(2)) 0.5) 2)))
;;                                   (range *num-node*)))
(defparameter *vertex-list* (mapcar (lambda (_)
                                          (make-vertex :pos (magicl:rand '(2))))
                                  (range *num-node*)))
(defparameter *edge-list* (mapcar (lambda (_)
                                          (make-edge :v (nth (random *num-center*) *vertex-list*)
                                                     :u (nth (random *num-node*) *vertex-list*)
                                                     :w (random 10)))
                                  (range *num-edges*)))
; (write *edge-list*)

(defun norm-vec (vec)
  (let ((n (magicl:norm vec)))
    (if (= n 0)
        vec
        (magicl:./ vec n))))


(magicl:norm (magicl:from-list '(1 1) '(2)))
(norm-vec (magicl:from-list '(1.0 1.0) '(2)))
(norm-vec (magicl:zeros '(2)))

(defun attractive-force (x &optional (k 20.1))
  (/ (expt x 2) k))

(defun repulsive-force (x &optional (k 0.01))
  (if (= x 0)
      0
      (/ (expt k 2) x)))

(attractive-force (magicl:norm (magicl:from-list '(1 1) '(2))))
(repulsive-force (magicl:norm (magicl:from-list '(1 1) '(2))))

(magicl:.* (norm-vec (magicl:rand '(2)))
           (repulsive-force (magicl:norm (magicl:from-list '(1 1) '(2)))))

(defun reset-node ()
  (dolist (e *edge-list*)
    (let ((v (edge-v e))
          (u (edge-u e)))
      (setf (vertex-pos v) (magicl:rand '(2)))
      (setf (vertex-pos u) (magicl:rand '(2)))
      ;; (setf (vertex-pos v) (.* (.- (magicl:rand '(2)) 0.5) 2))
      ;; (setf (vertex-pos u) (.* (.- (magicl:rand '(2)) 0.5) 2))
      )
    ))
; (reset-node)
(magicl:norm (from-list '(2 2) '(2)))

(defun force-directed-graph (&optional (iterantions 100))
  (let ((temperature 0.05d0))
    (dotimes (i iterantions)
      ;; replulsive forces
      (dolist (v *vertex-list*)
        (setf (vertex-disp v) (magicl:zeros '(2)))
        (dolist (u *vertex-list*)
          (let ((d (magicl:.- (vertex-pos v) (vertex-pos u))))
            (setf (vertex-disp v)  
                  (magicl:.+ (vertex-disp v)
                             (magicl:.* (norm-vec d) (repulsive-force (magicl:norm d)))))
            ))
        )
      ;; attractive forces
      (dolist (e *edge-list*)
        (let* ((v (edge-v e))
               (u (edge-u e))
               (d (magicl:.- (vertex-pos v) (vertex-pos u))))
          (setf (vertex-disp (edge-v e))
                (magicl:.- (vertex-disp (edge-v e))
                           (magicl:.* (norm-vec d) (attractive-force (magicl:norm d)))))
          (setf (vertex-disp (edge-u e))
                (magicl:.+ (vertex-disp (edge-u e))
                           (magicl:.* (norm-vec d) (attractive-force (magicl:norm d)))))
          ))
      ;; apply
      (dolist (v *vertex-list*)
        (let ((d (vertex-disp v))
              (pos (vertex-pos v)))
          (setf (vertex-pos v) (magicl:.+ (vertex-pos v)
                                          (magicl:.* (norm-vec d)
                                                     (min (magicl:norm d) temperature))))
          ;; (setf (vertex-pos v) (magicl:.+ (vertex-pos v)
          ;;                                 (magicl:.* (norm-vec d) (magicl:norm d))))
          (setf (tref (vertex-pos v) 0) (min 1.0d0
                                             (max 0.0d0
                                                  (tref (vertex-pos v) 0))))
          (setf (tref (vertex-pos v) 1) (min 1.0d0
                                             (max 0.0d0
                                                  (tref (vertex-pos v) 1))))
          ))
      )))

*vertex-list*
; (force-directed-graph)
; (reset-node)

(let* ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    (force-directed-graph 3)
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
