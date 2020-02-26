
(define *debug* #f)


(define (debug-print . args)
  (if *debug* (format #t "~a\n" args)))


(define (set-debug x) (set! *debug* x))

;; end of file
