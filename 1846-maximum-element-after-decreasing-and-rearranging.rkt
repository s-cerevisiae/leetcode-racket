#lang racket

(define (maximum-element-after-decrementing-and-rearranging arr)
  (define sorted (sort arr <))
  (let loop ([result 1] [lst (cdr sorted)])
    (match lst
      ['() result]
      [(cons next rest)
       (loop (if (< result next) (add1 result) result)
             rest)])))
