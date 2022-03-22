#lang racket

(require data/queue)

(define (vector-update! vec pos proc)
  (vector-set!
    vec pos
    (proc (vector-ref vec pos))))

(define (find-order course-num prerequisites)
  (define graph (make-vector course-num '()))
  (define indegrees (make-vector course-num 0))
  (for ([pair (in-list prerequisites)])
    (match-define (list c p) pair)
    (vector-update! graph p (curry cons c))
    (vector-update! indegrees c add1))

  (define to-visit (make-queue))
  (for ([c (in-naturals)]
        [i (in-vector indegrees)]
        #:when (zero? i))
    (enqueue! to-visit c))

  (define result
    (let loop ()
      (if (queue-empty? to-visit)
          '()
          (let ([c (dequeue! to-visit)])
            (for ([n (vector-ref graph c)])
              (vector-update! indegrees n sub1)
              (when (zero? (vector-ref indegrees n))
                (enqueue! to-visit n)))
            (cons c (loop))))))
  (if (= course-num (length result))
      result
      '()))
