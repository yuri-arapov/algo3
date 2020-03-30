

(use-modules (ice-9 receive))
(use-modules (srfi srfi-11))  ;; let-values

;; lazy union-find
(define (make-luf size)
  (vector
    ;; 0 - leader of i-th node (i at the beginning)
    (list->vector (iota (1+ size)))

    ;; 1 - rank of i-th leader (0 at the beginning)
    (make-vector (1+ size) 0)

    ;; 2 - number of domains (same as number of nodes at the beginning)
    size))


;; return number of domains in union-find structure
(define (luf-domain-count luf)
  (vector-ref luf 2))


;; return domain given node belongs to
(define *max-depth* 0)
(define *luf-find-count* 0)
(define (luf-find luf node)
  (let ((leaders (vector-ref luf 0)))
    (let ((ll (let loop ((n node) (depth 0))
                (let ((l (vector-ref leaders n)))
                  (if (= l n)
                      (begin
;;                        (set! *luf-find-count* (1+ *luf-find-count*))
;;                        (if (zero? (remainder *luf-find-count* 10000))
;;                            (format #t "****** luf-find depth ~d\n" depth))
                        (if (> depth 2)
                            (format #t "****** luf-find depth ~d\n" depth))
                        (if (> depth *max-depth*)
                            (begin
                              (format #t "****** luf-find max depth ~d\n" depth)
                              (set! *max-depth* depth)))
                        l)
                      (loop l (1+ depth)))))))
      (let compress ((n node))
        (let ((l (vector-ref leaders n)))
          (if (not (= l ll))
              (begin
                (vector-set! leaders n ll)
                (compress l)))))
      ll)))


;; merge two domains addressed by nodes n1 and n2
;; into single domain
(define (luf-union luf n1 n2)
  (let*
    ((leaders     (vector-ref luf 0))
     (leader-set! (lambda (n l) (vector-set! leaders n l)))
     (ranks       (vector-ref luf 1))
     (rank        (lambda (n) (vector-ref ranks n)))
     (rank-set!   (lambda (n r) (vector-set! ranks n r))))

    (vector-update! luf 2 1-) ;; decrease number of domains

    (let* ((d1 (luf-find luf n1))
           (r1 (rank n1))
           (d2 (luf-find luf n2))
           (r2 (rank n2)))
      (let-values (((winner looser-d looser-n)
                    (if (> r1 r2)
                        (values d1 d2 n2)
                        (values d2 d1 n1))))
        (leader-set! looser-d winner)
        (leader-set! looser-n winner)
        (if (= r1 r2)
            (rank-set! winner (1+ r1)))))))


;; end of file
