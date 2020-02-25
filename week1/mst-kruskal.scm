;; Minimum Spanning Tree by Kruskal's algorithm
;;

(load "union-find.scm")

(define (parse-edge-line line)
  (map string->number
       (filter (compose not string-null?)
               (string-split line #\space))))


(define (read-edges fname)
  (filter (lambda (el) (= 3 (length el)))
          (map parse-edge-line (read-file fname))))


(define (edge-from e) (car e))
(define (edge-to e)   (cadr e))
(define (edge-cost e) (caddr e))


(define (mst-kruskal edges)
  (let* ((uf (make-uf (length edges)))
         (loop? (lambda (node1 node2)
                  (= (uf-find uf node1) (uf-find uf node2)))))
    (fold
      (lambda (e res)
        (let ((n1 (edge-from e))
              (n2 (edge-to   e)))
          (if (not (loop? n1 n2))
              (begin
                (uf-union uf n1 n2)
                (+ res (edge-cost e)))
              res)))
      0
      (sort edges (lambda (e1 e2) (< (edge-cost e1) (edge-cost e2)))))))


;; -3612829
(define (mst)
  (mst-kruskal (read-edges "edges.txt")))

;; end of file
