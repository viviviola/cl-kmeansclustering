;;;; Implementation of the (naive/Lloyd's) k-means clustering algorithm in
;;;; Common Lisp!


;;; Might want to generate some 2D test data to experiment?
;;; Set up some 2D test data using three normally-distributed overlapping
;;; clusters (so if the algorithm is implemented correctly, we should
;;; roughly get these clusters back in the k-means).
(require :cl-random)

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
        (apply #'map (type-of (first values)) #'1d-mean values)))))

(defparameter *cluster-01* (make-2d-cluster 20 '(-1 0) '(2 2)))
(defparameter *cluster-02* (make-2d-cluster 20 '(0 1) '(1 2)))
(defparameter *cluster-03* (make-2d-cluster 20 '(1 1) '(2 2)))
(defparameter *dataset* (concatenate 'list
                                     *cluster-01*
                                     *cluster-02*
                                     *cluster-03*))

;;; So, given the data and an integer k for k means:
(defparameter k 3)

;;; 1. Generate an initial set of k means (using the Forgy method;
;;; just pick k of the data points to serve as the initial means)
(defparameter means (random-pick-without-replacement *dataset* k))

;;; 2. What distance metric are we using? (Euclidean, probably?)
(defun euclidean-dist (x y)
  "Return the Euclidean distance between points X and Y."
  (sqrt (reduce #'+ (map (type-of x) (lambda (xi) (expt xi 2))
                         (map (type-of x) #'- x y)))))


;;; Below all goes in a loop!


;;; 3. Given a point, find the closest mean
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

;;; 4. Assign each point to the cluster with the nearest mean
;;; (just use 0,1,...,k-1 indices for this I think)
(loop
   with means = (random-pick-without-replacement *dataset* k)
   and assigns = (make-list (length *dataset*))
   for index from 0 to (1- (length *dataset*))
   do (setf (elt assigns index) (closest-mean (elt *dataset* index) means))
   finally (return assigns))
     
;;; 5. And update the means for the next step.
;;; (Better way to write this?)
(loop for cluster-index from 0 to (1- k) collect
     (apply #'mean (loop
                      for index from 0 to (1- (length assigns))
                      when (= cluster-index (elt assigns index))
                      collect (elt *dataset* index))))

;;; 6. (TODO) Finally, put everything in a loop and terminate
;;; when the values don't change (so need to keep around the prev.
;;; k means or assigns or something.)
