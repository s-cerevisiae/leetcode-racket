#lang racket

(define (array-nesting nums)
  (define v (list->vector nums))
  (define visited (mutable-seteq))

  (for/fold ([max-len 0])
            ([i (in-range (vector-length v))]
             #:when (not (set-member? visited i)))
    (let loop ([ni i] [len 1])
      (set-add! visited ni)
      (define next (vector-ref v ni))
      (if (= next i)
          (max len max-len)
          (loop next (add1 len))))))
