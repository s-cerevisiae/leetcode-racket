#lang racket

(define (two-sum nums target)
  (define table (make-hash))
  (for/last ([i (in-naturals)]
             [x (in-list nums)]
             #:final (hash-has-key? table (- target x)))
    (hash-set! table x i)
    (list (hash-ref table (- target x)) i)))

(define (two-sum-func nums target)
  (let loop ([table (hash)] [i 0] [l nums])
    (match l
      ['() #f]
      [(cons x xs)
       (define maybe-pos (hash-ref table (- target x) #f))
       (if maybe-pos
           (list maybe-pos i)
           (loop (hash-set table x i) (add1 i) xs))])))
