#lang racket

(require "tree-node.rkt")

(define (add-one-row root val depth)
  (if (= depth 1)
      (tree-node val root #f)
      (let loop ([node root] [depth depth])
        (match node
          [(tree-node parent-val left right)
           (if (= depth 2)
               (tree-node
                 parent-val
                 (tree-node val left #f)
                 (tree-node val #f right))
               (tree-node
                 parent-val
                 (loop left (sub1 depth))
                 (loop right (sub1 depth))))]
          [#f #f]))))
