#lang racket

(require "tree-node.rkt")

(define (prune-tree node)
  (match node
    [#f #f]
    [(tree-node val left right)
     (let* ([new-left (prune-tree left)]
            [new-right (prune-tree right)]
            [new-node (tree-node val new-left new-right)])
       (if (zero? val)
           (and (or new-left new-right)
                new-node)
           new-node))]))
