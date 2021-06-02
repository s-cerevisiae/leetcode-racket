#lang racket

(module solution lazy
  (provide rob)

  (define (rob nums)
    (define len (length nums))
    (define dp
      (for/vector ([i (in-naturals)]
                   [x (in-list nums)])
        (cond [(= i 0) x]
              [(= i 1) (max (vector-ref dp 0) x)]
              [else (max (+ (vector-ref dp (- i 2)) x)
                         (vector-ref dp (- i 1)))])))
    (! (vector-ref dp (- len 1)))))

(require 'solution)
