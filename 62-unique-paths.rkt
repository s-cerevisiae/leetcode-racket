#lang racket

(define (unique-paths m n)
  (for/product ([x (in-range n (+ m (- n 1)))]
                [y (in-range 1 m)])
    (/ x y)))
