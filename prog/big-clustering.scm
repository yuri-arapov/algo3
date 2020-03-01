;; In this question your task is again to run the clustering
;; algorithm from lecture, but on a MUCH bigger graph. So big, in
;; fact, that the distances (i.e., edge costs) are only defined
;; implicitly, rather than being provided as an explicit list.
;; 
;; The data set is below.
;; 
;; clustering_big.txt
;; The format is:
;; 
;; [# of nodes] [# of bits for each node's label]
;; [first bit of node 1] ... [last bit of node 1]
;; [first bit of node 2] ... [last bit of node 2]
;; ...
;; 
;; For example, the third line of the file 
;; "0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1" denotes the
;; 24 bits associated with node #2.
;; 
;; The distance between two nodes uu and vv in this problem is
;; defined as the Hamming distance--- the number of differing
;; bits --- between the two nodes' labels. For example, the
;; Hamming distance between the 24-bit label of node #2 above and
;; the label "0 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1" is
;; 3 (since they differ in the 3rd, 7th, and 21st bits).
;; 
;; The question is: what is the largest value of kk such that
;; there is a kk-clustering with spacing at least 3? That is, how
;; many clusters are needed to ensure that no pair of nodes with
;; all but 2 bits in common get split into different clusters?
;; 
;; NOTE: The graph implicitly defined by the data file is so big
;; that you probably can't write it out explicitly, let alone
;; sort the edges by cost. So you will have to be a little
;; creative to complete this part of the question. For example,
;; is there some way you can identify the smallest distances
;; without explicitly looking at every pair of nodes?


(use-modules (ice-9 receive))

(load "bsearch.scm")
(load "combinations.scm")
(load "union-find.scm")
(load "debug-print.scm")


(define *zero* (char->integer #\0))

(define (parse-bit-line line)
  (map (lambda (c) (- (char->integer c) *zero*))
       (filter (lambda (c) (char<=? #\0 c #\1))
               (string->list line))))


;; read big clustering data
;; usage:
;; (receive (count bits-per-number data) (read-bit-graph fname)
;;   (do-somenting count)
;;   (do-yet more bits-per-number)
;;   ...)
;;
(define (read-bit-graph fname)
  (let* ((data (read-file fname))
         (count-size (map string->number (string-split (car data) #\space)))
         (bit-list (map parse-bit-line (cdr data))))
    (values (car count-size) (cadr count-size) bit-list)))



;; turn bit list into number
(define (bits->number bits)
  (fold (lambda (d res) (+ d (* res 2))) 0 bits))


;; size           - number of bits in the number
;; max-flip-count - how many bits to flip in the number
;;                  (up to starting from 1)
;; result         - list of bit indicies to flip
;;
;; example:
;; in 4-bit number filp only single bit
;; (make-flip-bits 4 1) ->
;;   ((0) (1) (2) (3))
;;
;; example:
;; in 4-bit nuber flip one or two bits
;; (make-flip-bits 4 2) ->
;;   ((0) (1) (2) (3) (0 1) (0 2) (0 3) (1 2) (1 3) (2 3))
(define (make-flip-bits size max-flip-count)
  (apply append (map (lambda (n) (combinations (iota size) n))
                     (iota max-flip-count 1))))


(define (bit->mask bit) (ash 1 bit))


(define (bits->mask bits)
  (fold (lambda (bit mask) (logior mask (bit->mask bit))) 0 bits))


(define (s2 . n)
  (cond ((null? n) "")
        ((null? (cdr n)) (number->string (car n) 2))
        (else (map (lambda (x) (number->string x 2)) n))))


(define (big-clustering node-count bits-per-node nodes max-hamming-distance)
  (let* ((flip-bits     (make-flip-bits  bits-per-node max-hamming-distance))
         (good-diffs    (list->vector (sort (map bits->mask flip-bits) <)))
         (good-diff?    (lambda (n1 n2) (bsearch good-diffs (logxor n1 n2))))
         (uf            (make-uf node-count))
         (numbers       (list->vector (map bits->number nodes)))
         (number        (lambda (idx) (vector-ref numbers idx)))
         (connected?    (lambda (n1 n2) (= (uf-find uf n1) (uf-find uf n2))))
         (connect       (lambda (n1 n2) (uf-union uf n1 n2))))
    (let loop ((indices (iota node-count)))
      (if (or (null? indices) (null? (cdr indices))) (uf-domain-count uf) ;; result
          (let* ((ihead (car indices))
                 (nhead (number ihead))
                 (tail  (cdr indices)))
            ;;(debug-print "head" (s2 nhead) "k" (uf-domain-count uf))
            (for-each
              (lambda (i)
                ;;(debug-print (s2 nhead) (s2 (number i)))
                (if (good-diff? nhead (number i))
                    (begin
                      (debug-print "match" ihead i (uf-domain-count uf) (s2 nhead (number i)))
                      (connect ihead i))))
              tail)
            (loop tail))))))


(define (week2-task2-with-file fname)
  (debug-print "reading data from file" fname)
  (receive (node-count bits-per-node nodes) (read-bit-graph fname)
     (debug-print "clustering")
     (big-clustering node-count bits-per-node nodes 2)))

(define (week2-task2)
  (week2-task2-with-file "clustering_big.txt"))

;; end of file
