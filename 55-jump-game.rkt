#lang racket

(define (can-jump nums)
  (define max-reach
    (for/fold ([reach 0])
              ([n (in-list nums)]
               [i (in-naturals)])
      #:break (< reach i)
      (max reach (+ n i))))
  (>= max-reach (sub1 (length nums))))
