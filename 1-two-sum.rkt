#lang typed/racket

(: two-sum ([Listof Integer] Integer -> [Listof Integer]))
(define (two-sum nums target)
  (define table (#{make-hash @ Integer Integer}))
  (or (for/last : (U False (List Integer Integer))
                 ([i (in-naturals)]
                  [x (in-list nums)]
                  #:final (hash-has-key? table (- target x)))
        (hash-set! table x i)
        (list (hash-ref table (- target x)) i))
      (list)))

(: two-sum-func ([Listof Integer] Integer -> [Listof Integer]))
(define (two-sum-func nums target)
  (let loop ([table (#{hash @ Integer Integer})] [i 0] [l nums])
    (match-define (cons x xs) l)
    (define maybe-pos (hash-ref table (- target x) #f))
    (if maybe-pos
        (list maybe-pos i)
        (loop (hash-set table x i) (add1 i) xs))))
