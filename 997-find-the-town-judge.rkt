#lang racket

(define (find-judge n trust)
  (match-define (cons in-degrees out-degrees)
    (foldl
      (match-lambda**
        [((list from to) (cons in out))
         (cons (hash-update in to add1 0)
               (hash-update out from add1 0))])
      (cons (hasheq) (hasheq))
      trust))
  (or (for/or ([x (in-inclusive-range 1 n)])
        (and (= (- n 1) (hash-ref in-degrees x 0))
             (= 0 (hash-ref out-degrees x 0))
             x))
      -1))
