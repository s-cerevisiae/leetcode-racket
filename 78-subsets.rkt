#lang racket/base

(define (subsets lst)
  (define v (list->vector lst))
  (define len (vector-length v))
  (for/list ([n (in-range (expt 2 len))])
    (for/list ([i (in-range (integer-length n))]
               #:when (bitwise-bit-set? n i))
      (vector-ref v i))))
