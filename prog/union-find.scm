

(use-modules (ice-9 receive))

(load "htlist.scm")


;; union-find
(define (make-uf size)
  (vector
    ;; 0 - number of nodes
    size

    ;; 1 - domain i-th node belongs to
    (list->vector (iota (1+ size)))

    ;; 2 - size of i-th domain
    ;;     initially size of every domain is 1
    (list->vector (make-list (1+ size) 1))

    ;; 3 - list of nodes of i-th domain
    ;;     initially i-th domain contains one member 'i'
    (list->vector (map (lambda (i) (list->htlist (list i))) (iota (1+ size))))

    ;; 4 - number of domains (same as number of nodes at the beginning)
    size))


;; return number of domains in union-find structure
(define (uf-domain-count uf)
  (vector-ref uf 4))


;; return domain given node belongs to
(define (uf-find uf node)
  (vector-ref (vector-ref uf 1) node))


;; merge two domains addressed by nodes n1 and n2
;; into single domain
(define (uf-union uf n1 n2)
  (let*
    ((domains             (vector-ref uf 1))
     (domain-sizes        (vector-ref uf 2))
     (members             (vector-ref uf 3))
     (get-node-domain     (lambda (node)        (vector-ref domains node)))
     (set-node-domain!    (lambda (node domain) (vector-set! domains node domain)))
     (get-domain-size     (lambda (domain)      (vector-ref domain-sizes domain)))
     (set-domain-size!    (lambda (domain size) (vector-set! domain-sizes domain size)))
     (get-domain-members  (lambda (domain)      (vector-ref members domain)))
     (set-domain-members! (lambda (domain ms)   (vector-set! members domain ms))))

    (vector-update! uf 4 1-) ;; decrease number of domains

    (let* ((d1 (get-node-domain    n1))
           (m1 (get-domain-members d1))
           (c1 (get-domain-size    d1))
           (d2 (get-node-domain    n2))
           (m2 (get-domain-members d2))
           (c2 (get-domain-size    d2)))
      (receive
        (winner winner-members looser looser-members)
          (if (> c1 c2)
            (values d1 m1 d2 m2)
            (values d2 m2 d1 m1))
          ;; update domain value of the looser
          (for-each (lambda (n) (set-node-domain! n winner))
                    (htlist->list looser-members))
          ;; update domain sizes
          (set-domain-size! winner (+ c1 c2))
          (set-domain-size! looser 0)
          ;; update domain members
          (set-domain-members! winner (htlist-append! winner-members looser-members))
          (set-domain-members! looser (list->htlist '()))))))


;; end of file
