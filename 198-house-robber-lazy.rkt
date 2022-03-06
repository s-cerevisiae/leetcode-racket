#lang racket

(define (rob nums)
  (define len (length nums))
  (define (dp-ref i)
    (force (vector-ref dp i)))
  (define dp
    (for/vector ([i (in-naturals)]
                 [x (in-list nums)])
      (cond [(= i 0) x]
            [(= i 1)
             (delay (max x (dp-ref 0)))]
            [else
             (delay
               (max (+ x (dp-ref (- i 2)))
                    (dp-ref (- i 1))))])))
  (dp-ref (- len 1)))
