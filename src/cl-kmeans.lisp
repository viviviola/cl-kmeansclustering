;;;; Implementation of the (naive/Lloyd's) k-means clustering algorithm
;;;; in Common Lisp!
;;;;
;;;; The exported function kmeans takes as input a given dataset (i.e.
;;;; a list of n-dimensional lists of real numbers) and a positive
;;;; integer k, and uses Lloyd's k-means clustering algorithm to
;;;; partition the dataset into k clusters, where each data point is
;;;; assigned to the cluster with the nearest mean (with distance
;;;; measured by the standard Euclidean metric in n-dimensions).
;;;;
;;;; By default, the algorithm loops for a maximum 20 iterations as
;;;; a safety. While mathematically k-means has to converge in a
;;;; finite number of steps, in practice the gains are typically
;;;; small after the first ten or so iterations. Users may freely
;;;; modify this maximum iteration count should they wish, by
;;;; changing the value of the optional parameter total-iterations.
;;;;
;;;; Example usage:
;;;;   ;; Make an example dataset
;;;;   (defparameter dataset '((1 1) (0 0.3) (0 0) (-1 2) (2 -1.2)
;;;;                           (2 0) (2 1) (1 1) (0.2 0.2) (-0.4 0)))
;;;;   ;; Divide the dataset into k=4 clusters
;;;;   (defparameter k 4)
;;;;   (kmeans dataset k)
;;;;   ;;> (2 1 1 1 0 3 2 2 1 1)

(defpackage :cl-kmeansclustering
  (:use :common-lisp)
  (:export :kmeans))
(in-package :cl-kmeansclustering)


;;; Typically use the Euclidean distance metric for point distances.
;;; (Other metrics are also useful for kmeans, e.g. the Mahalanobis metric,
;;; but are more computationally expensive. TODO: maybe implement these later?)
(defun euclidean-dist (x y)
  "Return the Euclidean distance between points X and Y."
  (sqrt (reduce #'+ (map (type-of x) (lambda (xi) (expt xi 2))
                         (map (type-of x) #'- x y)))))

(defun mean (&rest values)
  "Return the MEAN (average) of the given VALUES."
  ;; In n-dimensions we just do it component-wise, so have a helper function:
  (flet ((1d-mean (&rest 1d-values)
           (/ (reduce #'+ 1d-values) (length 1d-values))))
    (if (realp (first values))
        (apply #'1d-mean values)
        (apply #'map (type-of (first values)) #'1d-mean values))))

(defun random-pick-without-replacement (sequence &optional (n 1))
  "Randomly pick N different elements from the given SEQUENCE. (Default N=1.)"
  (cond ((> n (length sequence)) (error "N > number of items in SEQUENCE!"))
        ((= n 1) (elt sequence (random (length sequence))))
        (t (map (type-of sequence)
                (lambda (index) (elt sequence index))
                (loop
                   ;; TODO: Is there a better/more efficient way to do this?
                   with indices = nil
                   while (< (length indices) n)
                   do (setf indices
                            (remove-duplicates
                             (cons (random (length sequence))
                                   indices)))
                   finally (return indices))))))

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

(defun kmeans (dataset k &optional (total-iterations 20))
  "Find K clusters in the given DATASET using k-means clustering."
  ;; Generate an initial set of k means using Forgy's method
  (let ((means (random-pick-without-replacement dataset k))
        (assigns (make-list (length dataset)))
        (terminatep nil))
    (loop
       for iteration from 1 to total-iterations
       until terminatep do
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
           (setf means new-means)))
    assigns))
