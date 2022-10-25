;;;; Reference: https://ashok-khanna.medium.com/persistent-in-memory-data-storage-in-common-lisp-b-k-n-r-37f8ae76042f
(ql:quickload :bknr.datastore)


;; Add 'bknr.datastore:store-object' and 'bknr.datastore:persistent-class'
(defclass player (bknr.datastore:store-object)
  ((name :accessor name
           :initarg :name)
   (team :accessor team
         :initarg :team))
  (:metaclass bknr.datastore:persistent-class))


;; Define before load system
(make-instance 'bknr.datastore:mp-store
        :directory "/tmp/toy-algo/object-store/"
                         :subsystems (list
                                      (make-instance
           'bknr.datastore:store-object-subsystem)))

;; Add object
(make-instance 'player :name "hi0" :team "NYCU")
(make-instance 'player :name "hi1" :team "NYCU")
(make-instance 'player :name "hi2" :team "NYCU")


;; Get object
(bknr.datastore:all-store-objects)
;; Get object with class
(bknr.datastore:store-objects-with-class 'player)
;; Get all and print
(mapcar (lambda (x)
          (format t "(~A, ~A)~%" (name x) (team x)))
        (bknr.datastore:all-store-objects))


;; Modify
;; (bknr.datastore:with-transaction ()
;;   (setf (name
;;          (bknr.datastore:store-object-with-id 0))
;;          "No Name"))

;; Delete
;; (bknr.datastore:delete-object
;;  (bknr.datastore:store-object-with-id 0))


;; Close database
(bknr.datastore:close-store)


;;; Custom Indices
;; unique-index: Each value must be unique
;; hash-index: It can have multiple objects with the same value.
(defclass player-custom (bknr.datastore:store-object)
  ((name :accessor name
         :initarg :name
         :index-type bknr.indices:unique-index  ; unique-index
         :index-initargs (:test #'equal)
         :index-reader player-with-name
         :index-values all-names)
   (team :accessor team
         :initarg :team
         :index-type bknr.indices:hash-index  ; hash-index
         :index-initargs (:test #'equal)
         :index-reader player-with-team
         :index-values all-teams)
   )
  (:metaclass bknr.datastore:persistent-class))

(make-instance 'bknr.datastore:mp-store
        :directory "/tmp/toy-algo/object-store/"
                         :subsystems (list
                                      (make-instance
           'bknr.datastore:store-object-subsystem)))

(make-instance 'player-custom :name (format nil "~A" (random 1000000)) :team "NYCU")
(make-instance 'player-custom :name (format nil "~A" (random 1000000)) :team "NYCU")
(make-instance 'player-custom :name (format nil "~A" (random 1000000)) :team "NTHU")
(make-instance 'player-custom :name (format nil "~A" (random 1000000)) :team "NTHU")


(bknr.datastore:all-store-objects)
(bknr.datastore:store-objects-with-class 'player-custom)
(mapcar (lambda (x)
          (format t "(~A, ~A)~%" (name x) (team x)))
        (player-with-team "NYCU"))
(mapcar (lambda (x)
          (format t "(~A, ~A)~%" (name x) (team x)))
        (player-with-team "NTHU"))


;; Close database
(bknr.datastore:close-store)
