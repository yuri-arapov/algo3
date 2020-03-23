;; In this programming problem and the next you'll code up the greedy algorithm
;; from the lectures on Huffman coding.
;; 
;; Download the text file below.
;; huffman.txt
;;
;; This file describes an instance of the problem. It has the following format:
;; [number_of_symbols]
;; [weight of symbol #1]
;; [weight of symbol #2]
;; ...
;; 
;; For example, the third line of the file is "6852892," indicating that the
;; weight of the second symbol of the weights is 6852892. (We're using weights
;; instead of frequencies, like in the "A More Complex Example" video.)
;; 
;; Your task in this problem is to run the Huffman coding algorithm from
;; lecture on this data set. What is the maximum length of a codeword in the
;; resulting Huffman code?
;; 
;; ADVICE: If you're not getting the correct answer, try debugging your
;; algorithm using some small test cases. And then post them to the discussion
;; forum!


(use-modules (srfi srfi-11))  ;; let-values

(load "heap.scm")


(define (read-huffman file-name)
  (let* ((data (read-file-with file-name string->number))
         (count (car data))
         (weights (cdr data)))
    (values count weights)))


(define (make-symbol weight val encoding) (list weight val encoding))
(define (symbol-weight   s) (car s))
(define (symbol-value    s) (cadr s))
(define (symbol-encoding s) (caddr s))

(define (symbol-encoding-length s) (string-length (symbol-encoding s)))

(define (merge-symbols s1 s2)
  (make-symbol
    (+ (symbol-weight s1) (symbol-weight s2))
    (list s1 s2)
    ""))

(define (leaf-symbol? s) (not (list? (symbol-value s))))
(define (left-symbol s)  (car (symbol-value s)))
(define (right-symbol s) (cadr (symbol-value s)))

(define (symbol-weight<? s1 s2) (< (symbol-weight s1) (symbol-weight s2)))
(define (symbol-value<? s1 s2) (< (symbol-value s1) (symbol-value s2)))

(define (set-symbol-encoding s encoding)
  (make-symbol (symbol-weight s) (symbol-value s) encoding))


(define test-data '(5 25 32 20 18))


(define (huffman weights)
  (let* ((count (length weights))
         (hh    (make-heap count symbol-weight<?)))

    ;; put each symbol to heap
    (for-each
      (lambda (weight val)
        (heap-add hh (make-symbol weight val "")))
      weights (iota count))
    ;;;(print-heap hh 5 5)

    ;; build encoding tree bottom-to-top
    (dotimes (_ (1- count))
       ;; get two most inexpensive symbols, merge them into single one and put
       ;; it back to heap
       (heap-add
         hh
         (merge-symbols
           (heap-get hh)
           (heap-get hh))))
    ;;;(print-heap hh 5 5)

    (let* ((zero (char->integer #\0))
           (path->encoding
             (lambda (p)
               (list->string
                 (map (lambda (c) (integer->char (+ zero c)))
                      (reverse p)))))
           (root (heap-get hh)))

      ;; unfold tree into leaf encodings
      (letrec ((build-encoding
                 (lambda (s path res)
                   (if (leaf-symbol? s)
                     (cons (set-symbol-encoding s (path->encoding path))
                           res)
                     (let* ((ll (build-encoding (left-symbol s) (cons 0 path) res))
                            (rr (build-encoding (right-symbol s) (cons 1 path) ll)))
                       rr)))))
        (sort (build-encoding root '() '()) symbol-value<?)))))
        ;; sort to restore initial order


(define (task1-weights weights)
  (let loop ((ls (huffman weights))
             (smin #f)
             (smax #f))
    (if (null? ls)
      (if (not smin)
        (values smin smax)
        (values (symbol-encoding-length smin) smin
                (symbol-encoding-length smax) smax))
      (if (not smin)
        (loop (cdr ls) (car ls) (car ls))
        (let* ((s (car ls))
               (len (symbol-encoding-length s)))
          (loop (cdr ls)
                (if (< len (symbol-encoding-length smin)) s smin)
                (if (> len (symbol-encoding-length smax)) s smax)))))))


;;          weight  value  encoding
;; min  9 (7540662      0  "000011001")
;; max 19 (   1873    471  "1100111100110010100")
(define (task1)
  (let-values (((count data) (read-huffman "huffman.txt")))
    (task1-weights data)))

;; end of file
