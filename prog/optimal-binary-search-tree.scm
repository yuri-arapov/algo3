;; final exam task 10
;;

(define (make-matrix columns rows init-value)
  (let ((m (make-vector (* columns rows) init-value)))
    (vector columns rows m)))

(define (matrix-ref m c r)
  (let ((cols (vector-ref m 0))
        (rows (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-ref data (+ (* cols r) c))))

(define (matrix-set! m c r value)
  (let ((cols (vector-ref m 0))
        (rows (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-set! data (+ (* cols r) c) value)))


(define (optimal-bst weights)
  (let* ((pp (list->vector (cons #f weights)))
         (p (lambda (n) (vector-ref pp n)))

         (n (length weights))

         (range (lambda (x y) (iota (- y x -1) x)))
         ;; (range 1 4) -> '(1 2 3 4)

         (aa (make-matrix (+ n 1) (+ n 1) #f))
         (a  (lambda (i j) (if (> i j) 0
                             (matrix-ref aa i j))))
         (a! (lambda (i j x) (if (<= i j) (matrix-set! aa i j x))))

         (cost (lambda (r i s)
                 (+ (apply + (map p (range i (+ i s))))
                    (a i (- r 1))
                    (a (+ r 1) (+ i s))))))

    (dotimes (s 0 (- n 1))
      (dotimes (i 1 (- n s))
        (a! i (+ i s) (apply min (map (lambda (r) (cost r i s)) (range i (+ i s)))))))

    (a 1 n)))


;; final exam task 10 test data
(define task10-data '(0.20 0.05 0.17 0.10 0.20 0.03 0.25))


(define (task10)
  (optimal-bst task10-data))


;; end of file
