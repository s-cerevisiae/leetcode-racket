#lang racket

(define (max-ice-cream costs coins)
  (for/fold ([rem coins]
             [cnt 0]
             #:result cnt)
            ([cost (in-list (sort costs <))])
    #:break (< rem cost)
    (values (- rem cost)
            (add1 cnt))))
