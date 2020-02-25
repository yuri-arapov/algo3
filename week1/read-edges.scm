;; reading graph's edges from file


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


;; end of file
