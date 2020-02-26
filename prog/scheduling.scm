;; in this programming problem and the next you'll code up the
;; greedy algorithms from lecture for minimizing the weighted sum
;; of completion times..

;; jobs.txt is a data file.
;; This file describes a set of jobs with positive and integral
;; weights and lengths. It has the format
;; 
;; [number_of_jobs]
;; [job_1_weight] [job_1_length]
;; [job_2_weight] [job_2_length]
;; ...
;; For example, the third line of the file is "74 59", indicating
;; that the second job has weight 74 and length 59.
;; 
;; You should NOT assume that edge weights or lengths are distinct.
;; 
;; Your task in this problem is to run the greedy algorithm that
;; schedules jobs in decreasing order of the difference (weight -
;; length). Recall from lecture that this algorithm is not always
;; optimal. IMPORTANT: if two jobs have equal difference (weight
;; - length), you should schedule the job with higher weight
;; first. Beware: if you break ties in a different way, you are
;; likely to get the wrong answer. You should report the sum of
;; weighted completion times of the resulting schedule --- a
;; positive integer --- in the box below.
;; 
;; ADVICE: If you get the wrong answer, try out some small test
;; cases to debug your algorithm (and post your test cases to the
;; discussion forum).


(define *debug* #f)


(define (debug-print . args)
  (if *debug* (format #t "~a\n" args)))


(define (set-debug x) (set! *debug* x))


(define (job-weight j) (car j))
(define (job-length j) (cadr j))
(define (job-diff   j) (caddr j))
(define (job-ratio  j) (caddr j))


(define (parse-jobs-line line)
  (map string->number
       (filter (compose not string-null?)
               (string-split line #\space))))


(define (read-jobs fname)
  (filter (lambda (el) (= 2 (length el))) 
          (map parse-jobs-line (read-file fname))))


(define (make-job-diff j)
  (let ((w (job-weight j))
        (l (job-length j)))
    (list w l (- w l))))


(define (make-job-ratio j)
  (let ((w (job-weight j))
        (l (job-length j)))
    (list w l (/ w l))))


(define (job-diff>? j1 j2)
  (cond ((> (job-diff j1) (job-diff j2)) #t)
        ((< (job-diff j1) (job-diff j2)) #f)
        (else (cond ((> (job-weight j1) (job-weight j2)) #t)
                    ((< (job-weight j1) (job-weight j2)) #f)
                    ;;;(else (error "can not compare jobs" j1 j2))))))
                    (else #t)))))


(define (job>? j1 j2)
  (> (job-ratio j1) (job-ratio j2)))


(define (compute-weighted-completion-times-sum jobs)
  (debug-print jobs)
  (let loop ((res 0) (time 0) (jobs jobs))
    (debug-print res time)
    (if (null? jobs) res
        (let* ((j (car jobs))
               (t (+ time (job-length j))))
          (loop (+ res (* (job-weight j) t))
                t
                (cdr jobs))))))


(define (scheduling-impl jobs mode-proc >?-proc)
  (compute-weighted-completion-times-sum
    (sort (map mode-proc jobs) >?-proc)))


(define (task1-impl jobs) (scheduling-impl jobs make-job-diff  job-diff>?))
(define (task2-impl jobs) (scheduling-impl jobs make-job-ratio job>?))


;; 69119377652
(define (task1) (task1-impl (read-jobs "jobs.txt")))


;; 67311454237
(define (task2) (task2-impl (read-jobs "jobs.txt")))


;; end of file
