#lang racket

(define (rob nums)
  (for/fold ([p1 0] [p2 0]
             #:result p1)
            ([i (in-naturals)]
             [x (in-list nums)])
    (values
      (match i
        [0 x]
        [1 (max x p1)]
        [_ (max p1 (+ x p2))])
      p1)))
