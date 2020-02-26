;; In this programming problem you'll code up Prim's minimum
;; spanning tree algorithm.
;; 
;; edges.txt file describes an undirected graph with integer edge
;; costs. It has the format
;; 
;; [number_of_nodes] [number_of_edges]
;; [one_node_of_edge_1] [other_node_of_edge_1] [edge_1_cost]
;; [one_node_of_edge_2] [other_node_of_edge_2] [edge_2_cost]
;; ...
;; 
;; For example, the third line of the file is "2 3 -8874",
;; indicating that there is an edge connecting vertex #2 and
;; vertex #3 that has cost -8874.
;; 
;; You should NOT assume that edge costs are positive, nor should
;; you assume that they are distinct.
;; 
;; Your task is to run Prim's minimum spanning tree algorithm on
;; this graph. You should report the overall cost of a minimum
;; spanning tree --- an integer, which may or may not be negative
;; --- in the box below.
;; 
;; IMPLEMENTATION NOTES: This graph is small enough that the
;; straightforward O(mn) time implementation of Prim's algorithm
;; should work fine. OPTIONAL: For those of you seeking an
;; additional challenge, try implementing a heap-based version.
;; The simpler approach, which should already give you a healthy
;; speed-up, is to maintain relevant edges in a heap (with keys =
;; edge costs). The superior approach stores the unprocessed
;; vertices in the heap, as described in lecture. Note this
;; requires a heap that supports deletions, and you'll probably
;; need to maintain some kind of mapping between vertices and
;; their positions in the heap.


(load "read-edges.scm")


(define (count-graph-nodes edges)
  (fold
    (lambda (e max-node) (max max-node (edge-from e) (edge-to e)))
    0
    edges))

(define (make-graph edges)
  (let ((num-nodes (count-graph-nodes edges)))
    (vector
      edges                               ;; 0 - edges, list
      (make-vector (1+ num-nodes) #f))))  ;; 1 - nodes in use, vector

(define (graph-edges g) (vector-ref g 0))

(define (graph-num-nodes g) (1- (vector-length (vector-ref g 1))))

(define (graph-node-used? g node) (vector-ref (vector-ref g 1) node))

(define (graph-set-node-used g node) (vector-set! (vector-ref g 1) node #t))

(define (graph-crossing-edge? g e)
  (not (eq? (graph-node-used? g (edge-from e))
            (graph-node-used? g (edge-to   e)))))


(define (find-min-cost-edge g)
  (fold
    (lambda (e res)
      (if (and (graph-crossing-edge? g e)
               (or (not res) (< (edge-cost e) (edge-cost res))))
          e
          res))
    #f
    (graph-edges g)))


;; Prim's algorithm
(define (minimum-spanning-tree g)
  (if (zero? (graph-num-nodes g)) 0
    (begin
      (graph-set-node-used g 1)
      (let loop ((res 0) (num-nodes (1- (graph-num-nodes g))))
        (if (zero? num-nodes) res
          (let ((e (find-min-cost-edge g)))
            (graph-set-node-used g (edge-from e))
            (graph-set-node-used g (edge-to   e))
            (loop (+ res (edge-cost e)) (1- num-nodes))))))))


(define test-edges '(
  (1 2 1)
  (2 3 2)
  (3 4 3)
  (1 4 4)
  (1 3 5)))


;; -3612829
(define (task3)
  (minimum-spanning-tree (make-graph (read-edges "edges.txt"))))



;; end of file
