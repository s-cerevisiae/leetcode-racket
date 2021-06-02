#lang racket

(define (my-pow x n)
  ((if (< n 0) / values)
   (let loop ([x x] [n n] [result 1])
     (if (zero? n) result
       (loop (sqr x) (quotient n 2)
             (if (odd? n) (* x result) result))))))
