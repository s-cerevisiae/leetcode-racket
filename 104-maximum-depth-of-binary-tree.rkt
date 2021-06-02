#lang racket

(struct tree-node
  (val left right) #:mutable #:transparent)

(define (max-depth root)
  (let traverse ([node root] [depth 0])
    (if node
        (max (traverse (tree-node-left node) (add1 depth))
             (traverse (tree-node-right node) (add1 depth)))
        depth)))
