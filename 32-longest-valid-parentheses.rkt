#lang racket

(define (longest-valid-parentheses s)
  (for/fold ([stack '()]
             [carry -1]
             [result 0]
             #:result result)
            ([x (in-naturals)]
             [c (in-string s)])
    (match* (c stack)
      [(#\( _)
       (values (cons carry stack) x result)]
      [(#\) (cons y ns))
       (values ns y (max (- x y) result))]
      [(#\) '())
       (values stack x result)])))
