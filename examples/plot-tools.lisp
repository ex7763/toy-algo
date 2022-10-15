;;; Data
;; x-axis
(defparameter x
  (loop for i from 0 below 10 by 0.1
        collect i))

;; y-axis
(defparameter y
  (mapcar #'sin x))


;;; https://github.com/komi1230/kai
(ql:quickload :kai)  ; plotly binding

(defun kai-example ()

  (kai:line x y)
  (kai:title "test")
  (kai:show)
  )


;;; https://github.com/volkers/vgplot
(ql:quickload :vgplot)  ; gnuplot binding
(defun vgplot ()
    (vgplot:plot x y)
    (vgplot:axis (list 0 10 -1 1))
    (vgplot:xlabel "x")
    (vgplot:ylabel "y") 
  )


;;; https://github.com/bendudson/py4cl
;; More example https://github.com/bendudson/py4cl/tree/master/docs
;; import-module should be used as a top-level form, to ensure that the package is defined before it is used.
(ql:quickload :py4cl)
(py4cl:import-module "matplotlib")
(py4cl:import-module "matplotlib.pyplot" :as "plt")

(defun restart-python ()
  (py4cl:python-stop)
  (py4cl:python-start)
  (py4cl:python-exec "import matplotlib")
  (py4cl:python-exec "import matplotlib.pyplot as plt")
  (py4cl:python-alive-p)
  )

(defun py4cl-matplotlib ()
  ;(matplotlib:use "MacOSX")
  (plt:plot x y)
  (plt:xlabel "Time")

  (plt:show)
  (restart-python)
  )

;;; Run Example
; (kai-example)
; (vgplot)
; (py4cl-matplotlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/Lisp-Stat/plot
; (ql:quickload :lisp-stat)
; (ql:quickload :plot/vega)
