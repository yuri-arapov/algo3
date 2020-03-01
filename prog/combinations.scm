;; Return list of unique combinations of ls list elements by n.
;; Order of elements preserverd.
;; Exmaple:
;;   (combinations '(a b c) 2) ->
;;   ((a b) (a c) (b c))
;;
(define (combinations ls n)
  (define (iter ls n)
    (cond ((zero? n)
           '())
          ((< (length ls) n)
           '())
          ((= n 1)
           (map (lambda (x) (list x)) ls))
          ((= (length ls) n)
           (list ls))
          (else
            (let* ((r1 (iter (cdr ls) (- n 1)))
                   (r2 (iter (cdr ls) n)))
              (append
                (map (lambda (x) (cons (car ls) x)) r1)
                r2)))))
  (iter ls n))

;; end of file
