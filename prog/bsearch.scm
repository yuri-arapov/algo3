
;; binary search of number x in sorted vector vec
(define (bsearch vec x)
  (let search ((lo 0) (hi (- (vector-length vec) 1)))
    (if (> lo hi) #f
        (let* ((mi (quotient (+ lo hi) 2))
               (y (vector-ref vec mi)))
          (cond ((< y x) (search (+ mi 1) hi))
                ((> y x) (search lo (- mi 1)))
                (else #t))))))

;; end of file
