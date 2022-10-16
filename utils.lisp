(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string 
                 (mapcar #'symbol-name symbols))))
(funcall (symbol-append 'wr 'ite) "test symbol-append")

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun range-rec (start stop &key (step 1))
  "Recursive RANGE function"
  (when (and (> start stop) (plusp step))
    (error (format nil "Invalid positive step: start=~a, stop=~a, step=~a" start stop step)))
  (when (and (< start stop) (minusp step))
    (error (format nil "Invalid negative step: start=~a, stop=~a, step=~a" start stop step)))
  (when (zerop step)
    (error (format nil "Invalid zero step: start=~a, stop=~a, step=~a" start stop step)))
  (labels ((recur (i acc)
	     (cond
	       ((and (minusp step) (<= i stop)) (nreverse acc))
	       ((and (plusp step) (>= i stop)) (nreverse acc))
	       (t (recur (+ i step) (push i acc))))))
    (recur start nil)))
