;; htlist is a 'head-tail' list - the one that has both head and tail
;; pointers.
;; Objective is to have O(1) appending operation.


;; make htlist from list
(define (list->htlist s)
  (letrec ((tail (lambda (s)
                   (cond ((null? s) s)
                         ((null? (cdr s)) s)
                         (else (tail (cdr s)))))))

    (cons s (tail s)))) ;; pair: head and tail


;; return list from htlist
(define (htlist->list s) (car s))


;; retrun #t if htlist is empty
(define (htlist-null? s) (or (null? s) (null? (car s)) (null? (cdr s))))


;; append htlist s2 to htlist s1.
;; return new htlist containg concatenation of s1 and s2.
;; destructive for s1.
(define (htlist-append! s1 s2)
  (cond ((htlist-null? s1) s2)
        ((htlist-null? s2) s1)
        (else
          (set-cdr! (cdr s1) (car s2))
          (cons (car s1) (cdr s2)))))

;; end of file
