(load "glfw3-quadtree.asd")
(ql:quickload :glfw3-quadtree)
(asdf:make :glfw3-quadtree)

(load "separating-axis-theorem.lisp")
(sb-ext:save-lisp-and-die #p"separating-axis-theorem" :toplevel #'sat:main :executable t)

(quit)
