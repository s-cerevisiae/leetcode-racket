#lang racket

(require "tree-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (inorder-traversal root)
  (reset
    (let traverse ([node root])
      (match node
        [#f '()]
        [(tree-node val left right)
         (traverse left)
         (shift k (cons val (k (void))))
         (traverse right)]))))
