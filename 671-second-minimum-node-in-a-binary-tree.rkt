#lang racket

(require "tree-node.rkt")

(define (find-second-minimum-value root)
  (if root
      (or (search root (tree-node-val root))
          -1)
      -1))

(define (min/or x y)
  (if (and x y)
      (min x y)
      (or x y)))

(define (search node root-val)
  (match node
    [#f #f]
    [(tree-node val left right)
     (cond [(= val root-val)
            (min/or (search left root-val)
                    (search right root-val))]
           [(> val root-val) val]
           [else #f])]))
