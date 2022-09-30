;;;; quadtree.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

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

(defun draw-point (x0 y0 &optional (size 1))
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
  (when tree
    (apply #'draw-rect (quadtree-region tree))
    (if (> (quadtree-num-items tree) +quadtree-capacity+)
        (progn
          (draw-quadtree (quadtree-nw tree))
          (draw-quadtree (quadtree-ne tree))  
          (draw-quadtree (quadtree-sw tree))  
          (draw-quadtree (quadtree-se tree)))  
        (dolist (i (quadtree-item-list tree))
          (draw-point (item-x i) (item-y i))))
    )
  )

(defun where (tree x)
  (multiple-value-bind (x0 y0 x1 y1) 
      (values-list (quadtree-region tree))
    (if (< (item-x x) (+ x0 (/ (- x1 x0) 2)))
        (if (< (item-y x) (+ y0 (/ (- y1 y0) 2)))
            'nw
            'sw)
        (if (< (item-y x) (+ y0 (/ (- y1 y0) 2)))
            'ne
            'se)
      ) 
    )
  )

(defun if-not-exist-create-node (tree w)
  (multiple-value-bind (x0 y0 x1 y1) 
      (values-list (quadtree-region tree))
    (case w
      ('nw (progn (unless (quadtree-nw tree) 
                    (setf (quadtree-nw tree) 
                          (make-quadtree :region (mapcar #'+ (list 0 0 (/ (- x1 x0) 2) (/ (- y1 y0) 2))
                                                         (list x0 y0 x0 y0)))))
                  ))
      ('ne (progn (unless (quadtree-ne tree) 
                    (setf (quadtree-ne tree) 
                          (make-quadtree :region (mapcar #'+ (list (/ (- x1 x0) 2) 0 (- x1 x0) (/ (- y1 y0) 2))
                                                         (list x0 y0 x0 y0)))))
                  ))
      ('sw (progn (unless (quadtree-sw tree) 
                    (setf (quadtree-sw tree)
                          (make-quadtree :region (mapcar #'+ (list 0 (/ (- y1 y0) 2)
                                                                   (/ (- x1 x0) 2) (- y1 y0))
                                                         (list x0 y0 x0 y0)))))
                  ))
      ('se (progn (unless (quadtree-se tree) 
                    (setf (quadtree-se tree) 
                          (make-quadtree :region (mapcar #'+ (list (/ (- x1 x0) 2) (/ (- y1 y0) 2) 
                                                                          (- x1 x0) (- y1 y0))
                                                         (list x0 y0 x0 y0)))))
                  ))
      ))
  )
(mapcar #'+ '(1 2 3) '(3 2 1))

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))
(funcall (symbol-append 'wr 'ite) "test symbol-append")

(defgeneric insert (tree x))

(defmethod insert ((tree quadtree) (x item))
  (let ((w (where tree x))
        (region (quadtree-region tree)))
    (multiple-value-bind (x0 y0 x1 y1) (values-list region)
      (if (> (quadtree-num-items tree)  +quadtree-capacity+)
          (progn
            (incf (quadtree-num-items tree)) 
            (if-not-exist-create-node tree w)
            (case w
              ('nw (insert (quadtree-nw tree) x))
              ('ne (insert (quadtree-ne tree) x))
              ('sw (insert (quadtree-sw tree) x))
              ('se (insert (quadtree-se tree) x))
              )
            ;(insert (funcall (symbol-append 'quadtree- w) tree) x)
            )  
          (if (< (quadtree-num-items tree) +quadtree-capacity+)
              (progn
                (pushnew x (quadtree-item-list tree))
                (incf (quadtree-num-items tree)))
              ;; equal +quadtree-capacity+, split
              (progn
                (pushnew x (quadtree-item-list tree))
                (incf (quadtree-num-items tree)) 

                (dolist (_item (quadtree-item-list tree))
                  (if-not-exist-create-node tree (where tree _item))
                  (case (where tree _item)
                    ('nw (insert (quadtree-nw tree) _item))
                    ('ne (insert (quadtree-ne tree) _item))
                    ('sw (insert (quadtree-sw tree) _item))
                    ('se (insert (quadtree-se tree) _item))
                    )
                  ;(insert (funcall (symbol-append 'quadtree- (where tree _item)) tree) _item)
                  )
                (setf (quadtree-item-list tree) nil)

                (write "split"))
            )
        )
      ))
  )
(dolist (x '(1 2 3))
  (write x))

(insert (make-quadtree :region '(0 0 100 100))
        (make-item :x 10 :y 10))

(where (make-quadtree :region '(0 0 100 100))
        (make-item :x 10 :y 10))
(where (make-quadtree :region '(0 0 100 100))
        (make-item :x 60 :y 10))
(where (make-quadtree :region '(0 0 100 100))
        (make-item :x 10 :y 60))
(where (make-quadtree :region '(0 0 100 100))
        (make-item :x 60 :y 60))

(defparameter *root* (make-quadtree :region (list 0 0 +screen-width+ +screen-height+)))
*root*
;; (let ((_item (make-item :x 10 :y 10)))
;;   (if-not-exist-create-node *root* 'nw)
;;   ;(write (where *root* _item))
;;   (write (symbol-append 'quadtree- (where *root* _item))  )
;;   (write (funcall (symbol-append 'quadtree- (where *root* _item)) *root*))
;;   (insert (funcall (symbol-append 'quadtree- (where *root* _item)) *root*) _item)
;;   ;(insert (funcall (symbol-append 'quadtree- 'ne) *root*) _item)
;;   )

(let ((click nil))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)
    (when (and *buttons-pressed* (not click))
      ; (draw-point 200 200)
      (apply #'draw-point (get-cursor-position))
      (multiple-value-bind (x y) 
          (values-list (get-cursor-position))
        (pushnew (make-item :x x :y y) *item-list*)
        (insert *root* (make-item :x x :y y))
        )
      )
    (draw-quadtree *root*)
    ;; (mapcar (lambda (item)
    ;;           (draw-point (item-x item) (item-y item)))
    ;;         *item-list*)
    (setf click *buttons-pressed*)
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
