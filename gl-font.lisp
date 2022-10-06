;;;; cl-font-test.lisp
;;;; Author: Bo-Jiun Hsu

;; (ql:quickload '(cl-opengl cl-glfw alexandria trivial-main-thread))
;; (defpackage #:cl-glfw3-examples
;;   (:use #:cl #:glfw #:alexandria #:trivial-main-thread))

(ql:quickload :cl-glfw3-examples)
(ql:quickload :zpb-ttf)
(ql:quickload :cl-glu)
;(in-package :cl-glfw3-examples)
(defpackage :cl-font-test
  ;(:use :cl :cl-user :cl-glfw3 :cl-glfw3-examples)
  (:use :cl :cl-user :glfw :cl-glu :alexandria :trivial-main-thread)
  (:export :main))
(in-package :cl-font-test)

(defconstant +screen-width+ 512)
(defconstant +screen-height+ 512)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)


;; source: https://nklein.com/2010/01/better-text-rendering-with-cl-opengl-and-zpb-ttf/
(defun draw-quad (bx1 by1 bx2 by2)
  (gl:with-primitives :quads
    (gl:vertex bx1 by1)
    (gl:vertex bx2 by1)
    (gl:vertex bx2 by2)
    (gl:vertex bx1 by2)))

(defun make-interpolator (ss cc ee)
  (let ((xx (+ ss (* -2 cc) ee))
	(yy (* 2 (- cc ss)))
	(zz ss))
    #'(lambda (tt)
	(+ (* xx tt tt) (* yy tt) zz))))

(defun interpolate (sx sy ex ey int-x int-y cutoff &optional (st 0) (et 1))
  (let ((mx (/ (+ sx ex) 2.0))
	(my (/ (+ sy ey) 2.0))
	(mt (/ (+ st et) 2.0)))
    (let ((nx (funcall int-x mt))
	  (ny (funcall int-y mt)))
      (let ((dx (- mx nx))
	    (dy (- my ny)))
	(when (< (* cutoff cutoff) (+ (* dx dx) (* dy dy)))
	  (interpolate sx sy nx ny int-x int-y cutoff st mt)
	  (gl:vertex nx ny)
	  (interpolate nx ny ex ey int-x int-y cutoff mt et))))))

(defun render-glyph (glyph mode cutoff)
  (zpb-ttf:do-contours (contour glyph)
    (gl:with-primitives mode
      (zpb-ttf:do-contour-segments (start ctrl end) contour
	(let ((sx (zpb-ttf:x start))
	      (sy (zpb-ttf:y start))
	      (cx (when ctrl (zpb-ttf:x ctrl)))
	      (cy (when ctrl (zpb-ttf:y ctrl)))
	      (ex (zpb-ttf:x end))
	      (ey (zpb-ttf:y end)))
	  (gl:vertex sx sy)
	  (when ctrl
	    (let ((int-x (make-interpolator sx cx ex))
		  (int-y (make-interpolator sy cy ey)))
	      (interpolate sx sy ex ey int-x int-y cutoff)))
	  (gl:vertex ex ey))))))

(defun render-string (string font-loader fill cutoff)
  (loop :for pos :from 0 :below (length string)
     :for cur = (zpb-ttf:find-glyph (aref string pos) font-loader)
     :for prev = nil :then cur
     :do (when prev
	   (gl:translate (- (zpb-ttf:kerning-offset prev cur font-loader)
			    (zpb-ttf:left-side-bearing cur))
			 0 0))
	 (render-glyph cur (if fill :polygon :line-strip) cutoff)
         (gl:translate (zpb-ttf:advance-width cur) 0 0)))

(defun calculate-cutoff (font-loader size)
  (gl:with-pushed-matrix
    (let ((ss (/ size (zpb-ttf:units/em font-loader))))
      (gl:scale ss ss ss)

      (let ((modelview (gl:get-double :modelview-matrix))
	    (projection (gl:get-double :projection-matrix))
	    (viewport (gl:get-integer :viewport)))
	(labels ((dist (x1 y1 z1 x2 y2 z2)
		   (max (abs (- x1 x2))
			(abs (- y1 y2))
			(abs (- z1 z2))))
		 (dist-to-point-from-origin (px py pz ox oy oz)
		   (multiple-value-bind (nx ny nz)
		       (glu:un-project px py pz
				       :modelview modelview
				       :projection projection
				       :viewport viewport)
		     (dist nx ny nz ox oy oz))))
	  (multiple-value-bind (ox oy oz)
	      (glu:un-project 0.0 0.0 0.0
			      :modelview modelview
			      :projection projection
			      :viewport viewport)
	    (/ (min (dist-to-point-from-origin 1 0 0 ox oy oz)
		    (dist-to-point-from-origin 0 1 0 ox oy oz))
	       2)))))))

(defun draw-string (font-loader string &key (size 48) (filled t)
		                            (cutoff nil))
  (unless cutoff
    (setf cutoff (calculate-cutoff font-loader size)))

  (gl:with-pushed-matrix
    (let* ((box (zpb-ttf:string-bounding-box string font-loader :kerning t))
	   (bx1 (aref box 0))
	   (by1 (aref box 1))
	   (bx2 (aref box 2))
	   (by2 (aref box 3)))

      (let ((ss (/ size (zpb-ttf:units/em font-loader))))
	(gl:scale ss ss ss))

      (gl:translate (/ (- bx1 bx2) 2) (/ (- by1 by2) 2) 0)

      (gl:with-pushed-attrib (:current-bit :color-buffer-bit :line-bit
				           :hint-bit :stencil-buffer-bit)
	;; antialias lines
	(gl:enable :blend)
	(gl:blend-func :src-alpha :one-minus-src-alpha)
	(gl:enable :line-smooth)
	(gl:hint :line-smooth-hint :nicest)
	(gl:with-pushed-matrix
	    (render-string string font-loader nil cutoff))

	(when filled
	  ;; fill stencil buffer with filled-in-glyph
	  (gl:color-mask nil nil nil nil)
	  (gl:enable :stencil-test)
	  (gl:stencil-mask 1)
	  (gl:clear-stencil 0)
	  (gl:clear :stencil-buffer-bit)
	  (gl:stencil-func :always 1 1)
	  (gl:stencil-op :invert :invert :invert)
	  (gl:with-pushed-matrix
	      (render-string string font-loader t cutoff))

	  ;; fill in area subject to stencil
	  (gl:color-mask t t t t)
	  (gl:stencil-func :equal 1 1)
	  (draw-quad bx1 by1 bx2 by2)))))
  cutoff)

(let* ((click nil)
       (font "fonts/open-sans/OpenSans-Regular.ttf")
       (font-size 54)
       (last-time (%glfw:get-time))
       (now-time (%glfw:get-time))
       (fps 0)
       (font-loader (zpb-ttf:open-font-loader font)))
  (defun render ()
    (gl:clear :color-buffer)
    (gl:color 1.0 0.0 0.0)

    (gl:with-pushed-matrix
      (gl:translate 260  (/ +screen-height+ 2)  0)
      (gl:scale 1 -1 1)
      (draw-string font-loader "Hello Lisp" :size 54 :filled t)
      )
    (gl:with-pushed-matrix
      (gl:translate 20 32 0)
      (gl:scale 1 -1 1)
      (setf now-time (%glfw:get-time))
      (setf fps (+ (* fps 0.95)
                   (* (/ 1 (- now-time last-time)) 0.05)))
      (draw-string font-loader (format nil "~,0f" fps) :size 32 :filled t)
      (setf last-time now-time)
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
(funcall (find-symbol "QUIT" "COMMON-LISP-USER"))
