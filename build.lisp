(load "glfw3-quadtree.asd")
(ql:quickload :glfw3-quadtree)
(asdf:make :glfw3-quadtree)

(load "separating-axis-theorem.lisp")
#-(or windows win32) (sb-ext:save-lisp-and-die #p"separating-axis-theorem" :toplevel #'sat:main :executable t)
#+(or windows win32) (sb-ext:save-lisp-and-die #p"separating-axis-theorem.exe" :toplevel #'sat:main :executable t)


(quit)
