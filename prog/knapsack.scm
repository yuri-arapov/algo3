;; In this programming problem and the next you'll code up the knapsack
;; algorithm from lecture.
;; 
;; Let's start with a warm-up. Download the text file below.
;; 
;; knapsack1.txt
;; This file describes a knapsack instance, and it has the following format:
;; 
;; [knapsack_size][number_of_items]
;; [value_1] [weight_1]
;; [value_2] [weight_2]
;; ...
;; 
;; For example, the third line of the file is "50074 659", indicating that the
;; second item has value 50074 and size 659, respectively.
;; 
;; You can assume that all numbers are positive. You should assume that item
;; weights and the knapsack capacity are integers.
;; 
;; In the box below, type in the value of the optimal solution.
;; 
;; ADVICE: If you're not getting the correct answer, try debugging your
;; algorithm using some small test cases. And then post them to the discussion
;; forum!


(load "knapsack1-data.scm")


(define test-data '(
  6         ;; size of the knapsack
  (3 4)     ;; (value size) of the 1-st item
  (2 3)     ;; ...
  (4 2)     ;; ...
  (4 3)))   ;; (value size) of the last item

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


(define (knapsack-packing size items)
  (let* ((n  (length items))
         (vw (list->vector (cons #f items)))
         (vi (lambda (i) (car (vector-ref vw i))))
         (wi (lambda (i) (cadr (vector-ref vw i))))
         (w  size)
         (aa (make-matrix (1+ n) (1+ w) #f))
         (cc 0)
         (zz 0)
;;         (a  (lambda (i x)   (matrix-ref aa i x)))
;;         (a! (lambda (i x v) (matrix-set! aa i x v)
;;               (if (not (zero? v))
;;                 (set! cc (1+ cc))
;;                 (set! zz (1+ zz)))))

         (a  (lambda (i x) (let ((v (matrix-ref aa i x)))
                             (if v v 0))))
         (a! (lambda (i x v) (if (not (zero? v))
                               (begin 
                                 (matrix-set! aa i x v) 
                                 (set! cc (1+ cc)))
                               (begin 
                                 (set! zz (1+ zz))))))
         )
    (dotimes (x 0 w)
      (a! 0 x 0))
    (dotimes (i 1 n)
      (let ((vi  (vi i))
            (wi  (wi i))
            (i-1 (- i 1)))
        (dotimes (x 0 w)
          (a! i x
              (if (< x wi)
                (a i-1 x)
                (max (a i-1 x) (+ (a i-1 (- x wi)) vi)))))))
    (format #t "cc ~a zz ~a (~a)\n" cc zz (+ cc zz))
    (a n w)))


(define (test)
  (knapsack-packing (car test-data) (cdr test-data)))

;; time: 1.86 sec
;; 2493893
(define (task1)
  (knapsack-packing
    (car knapsack1-data)
    (cdr knapsack1-data)))


;; end of file
