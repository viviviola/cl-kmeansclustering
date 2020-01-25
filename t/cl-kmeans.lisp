;;;; Unit tests, etc. Using the FiveAM unit testing framework.
;;;; TODO: maybe also code coverage with sb-cover at some point?

;; Load in function definitions from src/cl-kmeans.lisp
(load "../src/cl-kmeans.lisp")
(in-package :cl-kmeansclustering)

;; Using the FiveAM unit testing framework
(require :fiveam)
(use-package :fiveam)


;;; Unit testing for cl-kmeansclustering:euclidean-dist
(def-suite test-euclidean-dist
    :description "Unit testing for #'euclidean-dist")
(in-suite test-euclidean-dist)

(test test-1a
      "Example test. They'll all pretty much look like this."
      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))



;;; Unit testing for cl-kmeansclustering:mean
(def-suite test-mean
    :description "Unit testing for #'mean")
(in-suite test-mean)

(test test-1b
      "Example test. They'll all pretty much look like this."
      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))


;;; Unit testing for cl-kmeansclustering:random-pick-without-replacement
(def-suite test-random-pick-without-replacement
    :description "Unit testing for #'random-pick-without-replacement")
(in-suite test-random-pick-without-replacement)

(test test-1c
      "Example test. They'll all pretty much look like this."
      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))


;;; Unit testing for cl-kmeansclustering:closest-mean
(def-suite test-closest-mean
    :description "Unit testing for #'closest-mean")
(in-suite test-closest-mean)

(test test-1d
      "Example test. They'll all pretty much look like this."
      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))


;;; Unit testing for cl-kmeansclustering:kmeans
(def-suite test-kmeans
    :description "Unit testing for #'kmeans")
(in-suite test-kmeans)

(test test-1e
      "Example test. They'll all pretty much look like this."
      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))







;;; Define the tests
;;; TODO: sort out how I want to import the functions/class from
;;;   ../t/cl-kiwi.
;;;   do I want it all packaged+exported after all?
;(test test-1
;      "Example test. They'll all pretty much look like this."
;      (is (= 4 (+ 2 2)) "2+2=4 test failed (using #'= to test equality)"))

;(test test-2
;      "Also make sure the methods/functions error when they're supposed to:"
;      (signals
;       (error "Adding an integer to a symbol didn't signal an error!")
;       (+ 2 'symbol)))

;;; Run the tests!
(run! '(test-euclidean-dist
        test-mean
        test-random-pick-without-replacement
        test-closest-mean
        test-kmeans))








;;; Might want to generate some 2D test data to experiment?
;;; Set up some 2D test data using three normally-distributed overlapping
;;; clusters (so if the algorithm is implemented correctly, we should
;;; roughly get these clusters back in the k-means).
;(require :cl-random)

;;; Using the vgplot interface to GNUplot
;(require :vgplot)

;(defun make-2d-cluster (n &optional (mean '(0.0d0 0.0d0)) (var '(1.0d0 1.0d0)))
;  "Generate a dataset of N 2d points (with given MEAN and VAR)."
;  (let ((rand-x (cl-random:r-normal (car mean) (car var)))
;        (rand-y (cl-random:r-normal (cadr mean) (cadr var))))
;    (loop for index from 1 to n collect
;         (list (cl-random:draw rand-x) (cl-random:draw rand-y)))))

;(defun nshuffle-list (list)
;  "Randomly shuffle the elements of the given LIST (non-consing)."
;  (loop for index from (length list) downto 2 do
;       (rotatef (elt list (random index))
;                (elt list (1- index)))
;     finally (return list)))
  
;;; TESTING DATA (move all of this into t/cl-kmeans.lisp later!)
;(defparameter *cluster-01* (make-2d-cluster 20 '(-1 0) '(1 1)))
;(defparameter *cluster-02* (make-2d-cluster 20 '(1 2) '(1 1)))
;(defparameter *cluster-03* (make-2d-cluster 20 '(1 -1) '(1 0.5)))
;(defparameter *dataset* (nshuffle-list (concatenate 'list
;                                                    *cluster-01*
;                                                    *cluster-02*
;                                                    *cluster-03*)))


;;; VGPLOT to plot the data in a gnuplot window
;(vgplot:new-plot)
;(let ((x (mapcar #'car *dataset*))
;      (y (mapcar #'cadr *dataset*)))
;  (vgplot:plot x y "ok;")
;  (vgplot:xlabel "x")
;  (vgplot:ylabel "y")
;  (vgplot:print-plot #p"data.png"))

;(defparameter *k* 3)
;(let ((assigns (kmeans *dataset* *k*)))
;  (vgplot:new-plot)
;  (let ((clusters
;         (loop for cluster-index from 0 to (1- *k*) collect
;              (remove nil (mapcar (lambda (clusterp points)
;                                    (and clusterp points))
;                                  (mapcar (lambda (cluster)
;                                            (= cluster cluster-index))
;                                          assigns)
;                                  *dataset*))))
;        (plot-strings '("or;" "ob;" "og;")))
;    ;; TODO: automate this with a zip or something.
;    (vgplot:plot (mapcar #'car (elt clusters 0))
;                 (mapcar #'cadr (elt clusters 0))
;                 (elt plot-strings 0)
;                 (mapcar #'car (elt clusters 1))
;                 (mapcar #'cadr (elt clusters 1))
;                 (elt plot-strings 1)
;                 (mapcar #'car (elt clusters 2))
;                 (mapcar #'cadr (elt clusters 2))
;                 (elt plot-strings 2))
;    (vgplot:xlabel "x")
;    (vgplot:ylabel "y")
;    (vgplot:print-plot #p"clustered-data.png")))
                 
