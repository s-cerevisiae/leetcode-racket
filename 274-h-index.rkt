#lang racket

(define (h-index citations)
  (foldl (λ (x h) (if (> x h) (+ h 1) h))
         0
         (sort citations >)))
