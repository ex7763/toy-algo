(in-package :asdf-user)

(defsystem "quadtree"
  :author "Bo-Jiun, Hsu"
  :version "0.0.1"
  :license "MIT"
  :description ""
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on (
   "cl-glfw3" "cl-opengl" "cl-glfw3-examples" "trivial-main-thread"
  )

  ;; Project stucture.
  :serial t
  :components ((:file "quadtree"))

  ;:defsystem-depends-on (:deploy)
  ;:build-operation "deploy-op"
  :build-operation "program-op"
  :build-pathname "quadtree"
  :entry-point "quadtree:main"

  ;; ;; Build a binary:
  ;; ;; don't change this line.
  ;; :build-operation "program-op"
  ;; ;; binary name: adapt.
  ;; :build-pathname "dumbbell"
  ;; ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  ;; :entry-point "dumbbell:main"
  )
