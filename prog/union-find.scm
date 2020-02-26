

;; union-find
(define (make-uf size)
  (vector
    size                            ;; 0 - size
    (list->vector (iota (1+ size))) ;; 1 - domain
    (make-vector (1+ size) 1)))     ;; 2 - count

;; return domain of the node
(define (uf-find uf node)
  (vector-ref (vector-ref uf 1) node))

;; merge two domains addressed by nodes node1 and node2
;; into single domain
(define (uf-union uf node1 node2)
  (let ((size   (vector-ref uf 0))
        (domain (vector-ref uf 1))
        (count  (vector-ref uf 2)))

    (let* ((d1 (vector-ref domain node1))
           (d2 (vector-ref domain node2))
           (c1 (vector-ref count d1))
           (c2 (vector-ref count d2)))

      (let ((winner (if (> c1 c2) d1 d2))
            (looser (if (> c1 c2) d2 d1)))
        (vector-set! count winner (+ c1 c2))
        (dotimes (node 1 size)
           (if (= (vector-ref domain node) looser)
             (vector-set! domain node winner)))))))


;; end of file
