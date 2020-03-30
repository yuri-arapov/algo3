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
(load "lazy-union-find.scm")
(load "debug-print.scm")


(define *lazy-union-find* #f)


(define make-uf         make-uf)
(define uf-find         uf-find)
(define uf-union        uf-union)
(define uf-domain-count uf-domain-count)


(if *lazy-union-find*
    (begin
      (format #t "\nlazy uinon-find\n")
      (set! make-uf         make-luf)
      (set! uf-find         luf-find)
      (set! uf-union        luf-union)
      (set! uf-domain-count luf-domain-count)))


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


(define (count-bits-set n)
  (let loop ((res 0) (n n))
    (if (zero? n) res
        (loop (if (= 1 (logand n 1)) (+ 1 res) res) (ash n -1)))))


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


;; wn is a 'weighted number' - number with the weight
;; where weight is amount of bits set to 1
(define (make-wn n) (cons n (count-bits-set n)))
(define (wn-n wn) (car wn))
(define (wn-w wn) (cdr wn))


(define (big-clustering node-count bits-per-node nodes max-hamming-distance)
  (let* ((flip-bits     (make-flip-bits  bits-per-node max-hamming-distance))
         (good-diffs    (list->vector (sort (map bits->mask flip-bits) <)))
         ;;(good-diff?    (lambda (x y) (bsearch good-diffs (logxor x y))))
         (good-diff?    (lambda (x y) (<= (count-bits-set (logxor x y)) 2)))
         (uf            (make-uf node-count))
         (find          (lambda (n) (uf-find uf n)))
         (wnumbers      (list->vector ;; sorted by weight
                          (sort
                            (map (lambda (bits) (make-wn (bits->number bits)))
                                 nodes)
                            (lambda (x y) (< (wn-w x) (wn-w y))))))
         (wn            (lambda (idx) (vector-ref wnumbers idx)))
         (number        (lambda (idx) (wn-n (wn idx))))
         (weight        (lambda (idx) (wn-w (wn idx))))
         ;;(connected?    (lambda (i j) (= (find i) (find j))))
         (connect       (lambda (i j) (uf-union uf i j)))
         (test-count    0))
    (let loop ((i 0))
      (if (< (- node-count i) 2) (uf-domain-count uf) ;; result
          (let ((ni (number i))
                (wi (weight i))
                (di (find i)))
            (let probe ((j (+ i 1)))
              (if (and (< j node-count)
                       (<= (abs (- wi (weight j))) max-hamming-distance))
                (begin
                  (if (and
                        (not (= di (find j))) ;; not connected
                        (good-diff? ni (number j))) ;; distance is good
                      (begin
                        ;;;(set! test-count (+ test-count 1))
                        (connect i j)
                        (set! di (find i)) ;; update di after merge
                        (debug-print "match" i j (uf-domain-count uf) (s2 ni (number j)))))
                  (probe (+ j 1)))))
            (if (< test-count 1000)
                (loop (+ i 1))))))))


(define (week2-task2-with-file fname)
  (debug-print "reading data from file" fname)
  (receive (node-count bits-per-node nodes) (read-bit-graph fname)
     (debug-print "clustering")
     (big-clustering node-count bits-per-node nodes 2)))


;; 6118 (time: 46302.86 sec)
;; 6118 (time: 42402.88 sec)
(define (week2-task2)
  (week2-task2-with-file "clustering_big.txt"))

;; end of file
