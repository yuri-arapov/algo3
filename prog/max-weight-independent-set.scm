;; In this programming problem you'll code up the dynamic programming algorithm
;; for computing a maximum-weight independent set of a path graph.
;; 
;; Download the text file below.
;; 
;; mwis.txt
;; This file describes the weights of the vertices in a path graph (with the
;; weights listed in the order in which vertices appear in the path). It has
;; the following format:
;; 
;; [number_of_vertices]
;; [weight of first vertex]
;; [weight of second vertex]
;; ...
;; 
;; For example, the third line of the file is "6395702," indicating that the
;; weight of the second vertex of the graph is 6395702.
;; 
;; Your task in this problem is to run the dynamic programming algorithm (and
;; the reconstruction procedure) from lecture on this data set. The question
;; is: of the vertices 1, 2, 3, 4, 17, 117, 517, and 997, which ones belong to
;; the maximum-weight independent set? (By "vertex 1" we mean the first vertex
;; of the graph---there is no vertex 0.) In the box below, enter a 8-bit
;; string, where the ith bit should be 1 if the ith of these 8 vertices is in
;; the maximum-weight independent set, and 0 otherwise. For example, if you
;; think that the vertices 1, 4, 17, and 517 are in the maximum-weight
;; independent set and the other four vertices are not, then you should enter
;; the string 10011010 in the box below.


(use-modules (srfi srfi-11))  ;; let-values

(load "debug-print.scm")


(define (read-mwis file-name)
  (let* ((data (read-file-with file-name string->number))
         (count (car data))
         (weights (cdr data)))
    (values count weights)))


(define test-data '(4 5 4 1))
(define test-data2 '(5 4 1 4))

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (2- n) (- n 2))

(define (max-weight-independent-set weights)
  (let* (
         ;; number of nodes
         (size (length weights))

         ;; vector of nodes weights
         (wvec (list->vector (cons #f weights)))

         ;; weight getter
         (w    (lambda (i) (vector-ref wvec i)))

         ;; memoisation vector
         (avec (make-vector (1+ size) #f))

         ;; memoisation getter
         (a    (lambda (i) (if (negative? i) 0 (vector-ref avec i))))

         ;; memoisation setter
         (aset!(lambda (i x) (vector-set! avec i x))))

    (debug-print "data" weights)
    ;; initialize A
    (aset! 0 0)
    (aset! 1 (w 1))

    ;; compute MWIS
    (dotimes (i 2 size)
      (aset! i (max (a (1- i)) (+ (a (2- i)) (w i)))))

    ;; reconstruction
    (debug-print avec)
    (letrec ((reconstruct
               (lambda (i res)
                 (debug-print "i" i "res " res)
                 (if (< i 1) res
                   (begin
                     (debug-print "a(i-1)" (a (1- i)) "a(i-2)" (a (2- i)) "w(i)" (w i))
                     (if (> (+ (a (2- i)) (w i))
                            (a (1- i)))
                       (reconstruct (2- i) (cons i res)) ;; i belongs to MWIS
                       (reconstruct (1- i) res)))))))

      (values
        (a size)                    ;; MWIS value
        (reconstruct size '())))))  ;; list of nodes that form MWIS



;; return list of same length as s.
;; i-th member of result is #t if i-th element of s is in ls and #f otherwise.
;; important: s and ls must be sorted in accending order.
;; example:
;;   (find-all '(1 2 3 4) '(0 2 4 6 8)) -> (#f #t #f #t)
(define (find-all s ls)
  (let loop ((s s) (ls ls) (res '()))
    ;;;(format #t "s ~a ls ~a res ~a\n" s ls res)
    (cond ((null? s) (reverse res))
          ((null? ls) (loop (cdr s) ls (cons #f res)))
          ((= (car s) (car ls)) (loop (cdr s) (cdr ls) (cons #t res)))
          ((> (car s) (car ls)) (loop s (cdr ls) res))
          (else (loop (cdr s) ls (cons #f res))))))


(define task-file "mwis.txt")
(define task-nodes '(1 2 3 4 17 117 517 997))


;; "10100110"
(define (task3)
  (debug-print "reading data from" task-file)
  (let-values (((count weights) (read-mwis task-file)))
    (debug-print "computing MWIS")
    (let-values (((max-sum nodes) (max-weight-independent-set weights)))
      (let ((bitstr (list->string
                      (map
                        (lambda (x) (if x #\1 #\0))
                        (find-all task-nodes nodes)))))
        (values task-nodes bitstr max-sum nodes)))))

;; end of file
