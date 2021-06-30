#lang racket

(require "tree-node.rkt")

(define (is-symmetric root)
  (check root root))

(define/match (check l r)
  [(#f #f) #t]
  [((tree-node x1 l1 r1)
    (tree-node x2 l2 r2))
   (and (= x1 x2)
        (check l1 r2)
        (check r1 l2))]
  [(_ _) #f])
