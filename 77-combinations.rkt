#lang racket

(define (combine n k)
  (define n-k (- n k))
  (define k-1 (- k 1))
  (define k* (build-vector k add1))
  (let loop ()
    (define index-to-change
      (for/first ([i (in-range k-1 -1 -1)]
                  #:when (not (eq? (vector-ref k* i) (+ i n-k 1))))
        i))
    (define combination (vector->list k*))
    (if index-to-change
        (let ([val-to-change (add1 (vector-ref k* index-to-change))])
          (for ([i (in-range index-to-change k)]
                [v (in-naturals val-to-change)])
            (vector-set! k* i v))
          (cons combination (loop)))
        (list combination))))
