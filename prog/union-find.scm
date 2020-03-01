

;; union-find
(define (make-uf size)
  (vector
    size                            ;; 0 - number of nodes
    (list->vector (iota (1+ size))) ;; 1 - domain given node belongs to
    (make-vector (1+ size) 1)       ;; 2 - number of nodes in domain
    size))                          ;; 3 - number of domains


;; return number of domains in union-find structure
(define (uf-domain-count uf)
  (vector-ref uf 3))


;; return domain of the node
(define (uf-find uf node)
  (vector-ref (vector-ref uf 1) node))

;; merge two domains addressed by nodes node1 and node2
;; into single domain
(define (uf-union uf node1 node2)
  (let ((size   (vector-ref uf 0))
        (domain (vector-ref uf 1))
        (count  (vector-ref uf 2)))

    (vector-update! uf 3 1-) ;; decrease number of domains

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
