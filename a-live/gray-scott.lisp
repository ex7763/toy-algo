;;;; gray-scott.lisp
;;;; Author: Bo-Jiun Hsu


(ql:quickload :cl-glfw3-examples)
(ql:quickload :magicl)
;(in-package :cl-glfw3-examples)
(defpackage :gray-scott
  ;(:use :cl :cl-user :cl-glfw3 :cl-glfw3-examples)
  (:use :cl :glfw :alexandria :trivial-main-thread)
  (:export :main))
(in-package :gray-scott)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *grid-size* 4)

(defun draw-rect (x0 y0 x1 y1)
  (; gl:with-primitive :line-loop
   gl:with-primitive :quads
    (gl:vertex x0 y0 0.0)
    (gl:vertex x0 y1 0.0)
    (gl:vertex x1 y1 0.0)
    (gl:vertex x1 y0 0.0)
    )
  )

(defun make-map (init-value &optional (value 0.5))
  (let* ((num-x-blocks (/ +screen-width+ *grid-size*)) 
         (num-y-blocks (/ +screen-height+ *grid-size*))
         (map (magicl:.* init-value
                         (magicl:ones (list num-x-blocks num-y-blocks)))))
    (dotimes (i (/ num-x-blocks 16))
      (dotimes (j (/ num-y-blocks 16))
        (setf (magicl:tref map 
                    (+ (/ num-x-blocks 2) i)
                    (+ (/ num-y-blocks 2) j))
              value)
      )
    )
    (setf map (magicl:.+ map 
                         (magicl:.* 0.1
                                    (magicl:rand (list num-x-blocks num-y-blocks)))))
    map
    ; (magicl:rand (list num-x-blocks num-y-blocks))
    )
  )


(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

; (reduce #'max (magicl::storage (magicl:rand '(10 10))) :key #'abs)
(defun draw-map (map)
  ;(gl:color 0.8 0.8 0.8)
  (let* ((x-list (range (nth 0 (magicl:shape map))))
         (x0-list (mapcar #'(lambda (x)
                              (* x *grid-size*))
                          x-list
                          )
                  )
         (y-list (range (nth 1 (magicl:shape map))))
         (y0-list (mapcar #'(lambda (y)
                              (* y *grid-size*))
                          y-list
                          ))
         (min-value (reduce #'min (magicl::storage map)))
         (max-value (reduce #'max (magicl::storage map))))
    (mapcar #'(lambda (x x0)
              (mapcar #'(lambda (y y0)
                        (gl:color (* 0.7
                                     (min (max (magicl:tref map x y) 0.0)
                                          1))
                                  0.1 
                                  0.1)
                        ;; (gl:color (* 0.7
                        ;;              (/ (- (magicl:tref map x y) min-value)
                        ;;                   (- max-value min-value)))
                        ;;           0.1 
                        ;;           0.1)
                        (draw-rect x0 y0 (+ x0 *grid-size*) (+ y0 *grid-size*))
                        )
                      y-list y0-list)
              )
            x-list x0-list
            )
    )
  )
;; (defun conv2d (input kernel &key (padding 0) (strides 1))
;;   (let* ((x-size (nth 0 (magicl:shape input)))
;;          (y-size (nth 1 (magicl:shape input)))
;;          (input-feature-map-size 1)
;;          (output-feature-map-size 1)
;;          (kernel-x-size (nth 0 (magicl:shape kernel)))
;;          (kernel-y-size (nth 1 (magicl:shape kernel)))
;;          (output-x-size (+ (/ (+ (- x-size kernel-x-size)
;;                                  (* 2 padding))
;;                               strides)
;;                            1))
;;          (output-y-size (+ (/ (+ (- y-size kernel-y-size)
;;                                  (* 2 padding))
;;                               strides)
;;                            1))
;;          (output (magicl:zeros (list output-x-size output-y-size)))
;;          )
;;     (dotimes (i output-feature-map-size)
;;       (dotimes (j input-feature-map-size)
;;         (dotimes (m output-x-size)
;;           (dotimes (n output-y-size)
;;             (dotimes (p kernel-x-size)
;;               (dotimes (q kernel-y-size)
;;                 (setf (magicl:tref output m n)
;;                       (+ 0 ; (magicl:tref output m n)
;;                          (* (magicl:tref kernel p q)
;;                             (magicl:tref input (min (max (- (+ m p) 1)
;;                                                          0)
;;                                                     (- x-size 1))
;;                                          (min (max (- (+ n q) 1)
;;                                                    0)
;;                                               (- y-size 1))
                                         
;;                                          )
;;                             ;; (magicl:tref input (- (+ m p) 1) 
;;                             ;;              (- (+ n q) 1))
;;                             ;;(magicl:tref input m n)
;;                             )
;;                          )
;;                       )
;;                 )
;;               )
;;             )
;;           )
;;         )
;;       )
;;     output
;;     )
;;   )

(defun conv2d (input kernel &key (padding 0) (strides 1))
  (let* ((x-size (nth 0 (magicl:shape input)))
         (y-size (nth 1 (magicl:shape input)))
         (input-feature-map-size 1)
         (output-feature-map-size 1)
         (kernel-x-size (nth 0 (magicl:shape kernel)))
         (kernel-y-size (nth 1 (magicl:shape kernel)))
         (output-x-size (+ (/ (+ (- x-size kernel-x-size)
                                 (* 2 padding))
                              strides)
                           1))
         (output-y-size (+ (/ (+ (- y-size kernel-y-size)
                                 (* 2 padding))
                              strides)
                           1))
         (output (magicl:zeros (list output-x-size output-y-size)))
         )
    (dotimes (m output-x-size)
      (dotimes (n output-y-size)
        (dotimes (p kernel-x-size)
          (dotimes (q kernel-y-size)
            (unless (or (> (+ m p (- padding)) (- x-size 1))
                        (< (+ m p (- padding)) 0)
                        (> (+ n q (- padding)) (- y-size 1))
                        (< (+ n q (- padding)) 0)
                        )
              (setf (magicl:tref output m n)
                    (+ (magicl:tref output m n)
                       (* (magicl:tref kernel p q)
                          (magicl:tref input
                                       (+ m p (- padding))
                                       (+ n q (- padding)))
                          )
                       )
                    )
              )
            )

          )
        )
      )
    output
    )
  )


(let* ((input (magicl:rand '(8 8)
                           :type 'double-float))
       ;(input (magicl:.* input 10))
       (a (progn (setf(magicl:tref input 4 4) 100)))
       (output 
         (conv2d input
                 (magicl:from-list '(0 -1 0
                                     -1 4 -1
                                     0 -1 0) 
                                   '(3 3)
                                   :type 'double-float)
                 ;; (magicl:from-list '(-1 -1 -1
                 ;;                        -1 8 -1
                 ;;                        -1 -1 -1) 
                 ;;                   '(3 3)
                 ;;                   :type 'double-float)
                 ;;(magicl:rand '(3 3))
                 :padding 1))
       )
  (format t "~A~%" input)
  (format t "~A~%" output)
  (format t "~A~%" (magicl:.+ input output))
  )

(defun laplacian (input)
    (conv2d input
            (magicl:from-list '(0 1 0
                                1 -4 1
                                0 1 0) 
                              '(3 3)
                              :type 'double-float)
            ;; (magicl:from-list '(-1 -1 -1
            ;;                        -1 8 -1
            ;;                        -1 -1 -1) 
            ;;                   '(3 3)
            ;;                   :type 'double-float)
            ;; (magicl:from-list '(1 1 1
            ;;                        1 -8 1
            ;;                        1 1 1) 
            ;;                   '(3 3)
            ;;                   :type 'double-float)
            :padding 1)
  )

;; (conv2d (magicl:rand '(128 128))
;;         (magicl:rand '(5 5))
;;         :padding 0)

(defun gray-scott (u v &key (f 0.04) (k 0.06))
  (let* ((dx 0.01)
         (dxdx (* dx dx))
         (fo 0.8)
         (Du 2e-5)
         (Dv 1e-5)
         (dt 1)
         ;; (dt (/ (* fo dxdx)
         ;;        (* 4 (max Du Dv))))
         (uvv (magicl:.* u v v))
         (laplacian-u (magicl:./ (laplacian u) dxdx))
         (laplacian-v (magicl:./ (laplacian v) dxdx))
         (dudt (magicl:.+ (magicl:.- (magicl:.* Du 
                                                laplacian-u)
                                     uvv
                                     )
                          (magicl:.* f
                                     (magicl:.- 1.0
                                                u))
                          ))
         ;(dudt  (magicl:.* Du laplacian-u))
         (dvdt (magicl:.- (magicl:.+ (magicl:.* Dv 
                                                laplacian-v)
                                     uvv
                                     )
                          (magicl:.* (+ f k)
                                     v))
               ))

    (setf u (magicl:.+ u
                       (magicl:.* dt dudt)))
    (setf v (magicl:.+ v
                       (magicl:.* dt dvdt)))

    (values u v)
    )
  )

(let ((u (magicl:rand '(8 8))) 
      (v (magicl:rand '(8 8))))
  (format t "~A~%" u)
  (format t "~A~%" v)
  (multiple-value-bind (new-u new-v) (gray-scott u v)
    (setf u new-u
          v new-v))
  (format t "~A~%" u)
  (format t "~A~%" v)
   )

; (magicl:normalize (magicl:zeros (list 10 10)))

(let ((click nil)
      (u (make-map 1.0 0.5))
      (v (make-map 0.0 0.25)))
  (defun render ()
    (gl:clear :color-buffer)
    (when (and *buttons-pressed* (not click))
      )
    (setf click *buttons-pressed*)

    (multiple-value-bind (new-u new-v)
        (gray-scott u v 
                    ; :f 0.04 :k 0.06
                    ; :f 0.022 :k 0.051
                    ; :f 0.025 :k 0.05
                     :f 0.025 :k 0.06
                    )
      (setf u new-u
            v new-v))
    (draw-map u)
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
    (with-init-window (:title "gray-scott" :width +screen-width+ :height +screen-height+)
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
