;; in this programming problem and the next you'll code up the
;; clustering algorithm from lecture for computing a max-spacing
;; k-clustering.
;; 
;; Download the text file below.
;; 
;; clustering1.txt
;; This file describes a distance function (equivalently, a
;; complete graph with edge costs). It has the following format:
;; 
;; [number_of_nodes]
;; [edge 1 node 1] [edge 1 node 2] [edge 1 cost]
;; [edge 2 node 1] [edge 2 node 2] [edge 2 cost]
;; ...
;; 
;; There is one edge (i,j)(i,j) for each choice of 1≤i<j≤n, where
;; nn is the number of nodes.
;; 
;; For example, the third line of the file is "1 3 5250",
;; indicating that the distance between nodes 1 and 3
;; (equivalently, the cost of the edge (1,3)) is 5250. You can
;; assume that distances are positive, but you should NOT assume
;; that they are distinct.
;; 
;; Your task in this problem is to run the clustering algorithm
;; from lecture on this data set, where the target number kk of
;; clusters is set to 4. What is the maximum spacing of a
;; 4-clustering?
;; 
;; ADVICE: If you're not getting the correct answer, try
;; debugging your algorithm using some small test cases. And then
;; post them to the discussion forum!
;;

(load "debug-print.scm")
(load "union-find.scm")
(load "read-edges.scm")


(define (max-spacing-clustering k edges)
  (let* ((size (count-graph-nodes edges))
         (uf (make-uf size))
         (same-cluster? (lambda (n1 n2) (= (uf-find uf n1) (uf-find uf n2))))
         (fuse (lambda (n1 n2) (uf-union uf n1 n2)))
         (sorted-edges (sort edges (lambda (e1 e2) (< (edge-cost e1) (edge-cost e2))))))
    (debug-print "size" size)
    (let loop ((num-clusters size) (edges sorted-edges))
      (let* ((e (car edges))
             (n1 (edge-from e))
             (n2 (edge-to e)))
        (if (same-cluster? n1 n2)
            (begin
              (debug-print "skip" n1 n2 "cost" (edge-cost e) "k" num-clusters)
              (loop num-clusters (cdr edges)))
            (begin
              (if (= num-clusters k)
                  (edge-cost e) ;; result found
                  (begin
                    (debug-print "fuse" n1 n2 "cost" (edge-cost e) "k" num-clusters)
                    (fuse n1 n2)
                    (loop (1- num-clusters) (cdr edges))))))))))

;; 106
(define (week2-task1)
  (max-spacing-clustering 4 (read-edges "clustering1.txt")))

;; end of file
