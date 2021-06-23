#lang racket

(require "tree-node.rkt")

(define (min-depth root)
  (let traverse ([node root] [depth 0])
    (match node
      [#f depth]
      [(or (tree-node _ #f n)
           (tree-node _ n #f))
       (traverse n (add1 depth))]
      [(tree-node _ l r)
       (min (traverse l (add1 depth))
            (traverse r (add1 depth)))])))
