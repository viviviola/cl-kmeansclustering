;;;; Implementation of the (naive/Lloyd's) k-means clustering algorithm in
;;;; Common Lisp!


;;; Might want to generate some 2D test data to experiment?
;;; Set up some 2D test data using three normally-distributed overlapping
;;; clusters (so if the algorithm is implemented correctly, we should
;;; roughly get these clusters back in the k-means).
(require :cl-random)

;;; Using the vgplot interface to GNUplot
(require :vgplot)

(defun make-2d-cluster (n &optional (mean '(0.0d0 0.0d0)) (var '(1.0d0 1.0d0)))
  "Generate a dataset of N 2d points (with given MEAN and VAR)."
  (let ((rand-x (cl-random:r-normal (car mean) (car var)))
        (rand-y (cl-random:r-normal (cadr mean) (cadr var))))
    (loop for index from 1 to n collect
         (list (cl-random:draw rand-x) (cl-random:draw rand-y)))))

(defun random-pick-without-replacement (sequence &optional (n 1))
  "Randomly pick N different elements from the given SEQUENCE. (Default N=1.)"
  (cond ((> n (length sequence)) (error "N > number of items in SEQUENCE!"))
        ((= n 1) (elt sequence (random (length sequence))))
        (t (map (type-of sequence)
                (lambda (index) (elt sequence index))
                (loop
                   ;; Is there a better/more efficient way to do this?
                   with indices = nil
                   while (< (length indices) n)
                   do (setf indices
                            (remove-duplicates
                             (cons (random (length sequence))
                                   indices)))
                   finally (return indices))))))

(defun mean (&rest values)
  "Return the MEAN (average) of the given VALUES."
  ;; In n-dimensions we just do it component-wise, so have a helper function:
  (flet ((1d-mean (&rest 1d-values)
           (/ (reduce #'+ 1d-values) (length 1d-values))))
    (if (realp (first values))
        (apply #'1d-mean values)
        (apply #'map (type-of (first values)) #'1d-mean values))))

;;; Typically use the Euclidean distance metric for point distances.
;;; (Other metrics are also useful for kmeans, e.g. the Mahalanobis metric,
;;; but are more computationally expensive. TODO: maybe implement these later?)
(defun euclidean-dist (x y)
  "Return the Euclidean distance between points X and Y."
  (sqrt (reduce #'+ (map (type-of x) (lambda (xi) (expt xi 2))
                         (map (type-of x) #'- x y)))))

(defun closest-mean (point means)
  "Return the index of the closest MEAN to POINT."
  (loop
     with closest-index = 0
     and distances = (mapcar #'euclidean-dist
                             (make-list (length means) :initial-element point)
                             means)
     for index from 1 to (1- (length means))
     when (< (elt distances index) (elt distances closest-index))
     do (setf closest-index index)
     finally (return closest-index)))

(defun nshuffle-list (list)
  "Randomly shuffle the elements of the given LIST (non-consing)."
  (loop for index from (length list) downto 2 do
       (rotatef (elt list (random index))
                (elt list (1- index)))
     finally (return list)))
  


;;; TESTING DATA (move all of this into t/cl-kmeans.lisp later!)
(defparameter *cluster-01* (make-2d-cluster 20 '(-2 0) '(0.5 0.5)))
(defparameter *cluster-02* (make-2d-cluster 20 '(1 2) '(1 1)))
(defparameter *cluster-03* (make-2d-cluster 20 '(1 -1) '(1 0.5)))
(defparameter *dataset* (nshuffle-list (concatenate 'list
                                                    *cluster-01*
                                                    *cluster-02*
                                                    *cluster-03*)))


;;; VGPLOT to plot the data in a gnuplot window
(vgplot:new-plot)
(let ((x (mapcar #'car *dataset*))
      (y (mapcar #'cadr *dataset*)))
  (vgplot:plot x y "ok;")
  (vgplot:xlabel "x")
  (vgplot:ylabel "y")
  (vgplot:print-plot #p"data.png"))

(defparameter *k* 3)
(let ((assigns (kmeans *dataset* *k*)))
  (vgplot:new-plot)
  (let ((clusters
         (loop for cluster-index from 0 to (1- *k*) collect
              (remove nil (mapcar (lambda (clusterp points)
                                    (and clusterp points))
                                  (mapcar (lambda (cluster)
                                            (= cluster cluster-index))
                                          assigns)
                                  *dataset*))))
        (plot-strings '("or;" "ob;" "og;")))
    ;; TODO: automate this with a zip or something.
    (vgplot:plot (mapcar #'car (elt clusters 0))
                 (mapcar #'cadr (elt clusters 0))
                 (elt plot-strings 0)
                 (mapcar #'car (elt clusters 1))
                 (mapcar #'cadr (elt clusters 1))
                 (elt plot-strings 1)
                 (mapcar #'car (elt clusters 2))
                 (mapcar #'cadr (elt clusters 2))
                 (elt plot-strings 2))
    (vgplot:xlabel "x")
    (vgplot:ylabel "y")
    (vgplot:print-plot #p"clustered-data.png")))
                 
       
(defun kmeans (dataset k &optional (total-iterations 50))
  "Find K clusters in the given DATASET using k-means clustering."
  ;; Generate an initial set of k means using Forgy's method
  (let ((means (random-pick-without-replacement dataset k))
        (assigns (make-list (length dataset)))
        (terminatep nil))
    (loop
       for iteration from 1 to total-iterations
       until terminatep do
         (format t "Iteration ~d~%" iteration)  ;; DEBUG
         ;; Assign each point to the cluster with the closest mean
         (loop for point-index from 0 to (1- (length dataset)) do
              (setf (elt assigns point-index)
                    (closest-mean (elt dataset point-index) means)))
         ;; Then update the means for the next iteration
         (let ((new-means
                (loop for cluster-index from 0 to (1- k) collect
                     (apply #'mean
                            (loop
                               for point-index from 0 to (1- (length assigns))
                               when (= cluster-index (elt assigns point-index))
                               collect (elt dataset point-index))))))
           (setf terminatep (equal means new-means))
           (format t "Means changed? ~a~%" (not terminatep))
           (setf means new-means)))
    assigns))
         
;; TODO: Some cleanup, perhaps?
