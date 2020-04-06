;; This problem also asks you to solve a knapsack instance, but a much bigger
;; one.
;; 
;; Download the text file below.
;; 
;; knapsack_big.txt
;; This file describes a knapsack instance, and it has the following format:
;; 
;; [knapsack_size][number_of_items]
;; [value_1] [weight_1]
;; [value_2] [weight_2]
;; ...
;; 
;; For example, the third line of the file is "50074 834558", indicating that
;; the second item has value 50074 and size 834558, respectively. As before,
;; you should assume that item weights and the knapsack capacity are integers.
;; 
;; This instance is so big that the straightforward iterative implemetation
;; uses an infeasible amount of time and space. So you will have to be creative
;; to compute an optimal solution. One idea is to go back to a recursive
;; implementation, solving subproblems --- and, of course, caching the results
;; to avoid redundant work --- only on an "as needed" basis. Also, be sure to
;; think about appropriate data structures for storing and looking up solutions
;; to subproblems.
;; 
;; In the box below, type in the value of the optimal solution.
;; 
;; ADVICE: If you're not getting the correct answer, try debugging your
;; algorithm using some small test cases. And then post them to the discussion
;; forum!


(use-modules (srfi srfi-69))


(load "knapsack1-data.scm")
(load "knapsack_big-data.scm")


(define test-data '(
  6         ;; size of the knapsack
  (3 4)     ;; (value size) of the 1-st item
  (2 3)     ;; ...
  (4 2)     ;; ...
  (4 3)))   ;; (value size) of the last item

(define 1st car)
(define 2nd cadr)

;; knapsack problem solution
;; recursive method
;; https://en.wikipedia.org/wiki/Knapsack_problem
(define (knapsack-packing size items)
  (let* ((n  (length items))

         (vw (list->vector (cons #f items)))
         (v  (lambda (i) (1st (vector-ref vw i))))
         (w  (lambda (i) (2nd (vector-ref vw i))))
         (W  size)

         (vv         (make-hash-table))
         (value      (lambda (i j) (hash-table-ref/default vv (cons i j) #f)))
         (value-set! (lambda (i j x) (hash-table-set! vv (cons i j) x)))

         (max-depth 0)
         (depth     0))

    (letrec (
     (m (lambda (i j)
          (set! depth (1+ depth))
          (if (> depth max-depth)
            (begin
              (set! max-depth depth)
              (format #t "max depth ~a\n" max-depth)))
          (if (or (zero? i) (negative? j))
            0
            (let ((i-1 (- i 1))       ;; just an eye candy
                  (j-wi (- j (w i)))) ;; (same here)
              (if (not (value i-1 j))
                (value-set! i-1 j (m i-1 j)))
              (if (> (w i) j)
                (value-set! i j (value i-1 j))
                (begin
                  (if (not (value i-1 j-wi))
                    (value-set! i-1 j-wi (m i-1 j-wi)))
                  (value-set! i j (max (value i-1 j) (+ (value i-1 j-wi) (v i))))))
              (set! depth (1- depth))
              (value i j))))))
      (let ((res (m n W)))
        (format #t "footprint ~a\n" (length (hash-table->alist vv)))
        res))))

(define (test)
  (knapsack-packing (car test-data) (cdr test-data)))

;; footprint 845939
;; time: 21.97 sec
;; $1 = 2493893
(define (task1)
  (knapsack-packing
    (car knapsack1-data)
    (cdr knapsack1-data)))

;; footprint 10629293
;; time: 165.41 sec
;; $2 = 4243395
(define (task2)
  (knapsack-packing
    (car knapsack_big-data)
    (cdr knapsack_big-data)))

;; end of file
