#lang racket

(define (merge-triplets triplets target)
  (match-define (list x y z) target)
  (define possible
    (filter
      (match-lambda
        [(list a b c) (and (<= a x) (<= b y) (<= c z))])
      triplets))
  (and (pair? possible)
       (equal? target (apply map max possible))))
