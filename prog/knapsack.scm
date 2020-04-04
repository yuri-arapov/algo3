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

(define (make-matrix rows columns init-value)
  (let ((m (make-vector (* rows columns) init-value)))
    (vector rows columns m)))

(define (matrix-ref m r c)
  (let ((rows (vector-ref m 0))
        (cols (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-ref data (+ (* rows r) c))))

(define (matrix-set! m r c value)
  (let ((rows (vector-ref m 0))
        (cols (vector-ref m 1))
        (data (vector-ref m 2)))
    (vector-set! data (+ (* rows r) c) value)))


(define (knapsak-packing size items)
  (let* ((n  (length items))
         (vw (list->vector (cons #f items)))
         (vi (lambda (i) (car (vector-ref vw i))))
         (wi (lambda (i) (cadr (vector-ref vw i))))
         (w  size)
         (aa (make-matrix (1+ n) (1+ w) #f))
         (a  (lambda (i x)   (matrix-ref aa i x)))
         (a! (lambda (i x v) (matrix-set! aa i x v))))
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
    (a n w)))


;; time: 1.74 sec
;; 2370623
(define (task1)
  (knapsak-packing
    (car knapsak1-data)
    (cdr knapsak1-data)))


;; end of file
