#lang racket

(define (num-rabbits answers)
  (define table (make-hash))
  (for-each
    (λ (x) (hash-update! table x add1 0))
    answers)
  (for/sum ([(ans cnt) (in-hash table)])
    (* (+ ans 1) (ceiling (/ cnt (+ ans 1))))))
