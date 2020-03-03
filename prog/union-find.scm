

(use-modules (ice-9 receive))

;; union-find
(define (make-uf size)
  (vector
    ;; 0 - number of nodes
    size

    ;; 1 - domain i-th node belongs to
    (list->vector (iota (1+ size)))

    ;; 2 - list of nodes of i-th domain
    ;;     initially i-th domain contains one member 'i'
    (list->vector (map (lambda (i) (make-vector 1 i)) (iota (1+ size))))

    ;; 3 - number of domains (same as number of nodes at the beginning)
    size))


;; return number of domains in union-find structure
(define (uf-domain-count uf)
  (vector-ref uf 3))


;; return domain of the node
(define (uf-find uf node)
  (vector-ref (vector-ref uf 1) node))


(define (append-vectors v1 v2)
  (list->vector (append (vector->list v1) (vector->list v2))))


;; merge two domains addressed by nodes n1 and n2
;; into single domain
(define (uf-union uf n1 n2)
  (let* ((size              (vector-ref uf 0))
         (domains           (vector-ref uf 1))
         (members           (vector-ref uf 2))
         (get-node-domain   (lambda (node) (vector-ref domains node)))
         (get-members       (lambda (domain) (vector-ref members domain)))
         (get-members-count (lambda (domain) (vector-length (get-members domain)))))

    (vector-update! uf 3 1-) ;; decrease number of domains

    (let* ((d1 (get-node-domain n1))
           (m1 (get-members     d1))
           (d2 (get-node-domain n2))
           (m2 (get-members     d2)))
      (receive
        (winner looser looser-members)
          (if (> (get-members-count d1) (get-members-count d2))
            (values d1 d2 m2)
            (values d2 d1 m1))
        (vector-set! members winner (append-vectors m1 m2))
        (vector-set! members looser (make-vector 0))
        (for-each
          (lambda (n) (vector-set! domains n winner))
          (vector->list looser-members))))))


;; end of file
