#lang racket

(require "tree-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (inorder-traversal root)
  (define (yield x)
    (shift k (cons x (k (void)))))
  (reset
    (let traverse ([node root])
      (match node
        [#f '()]
        [(tree-node val left right)
         (traverse left)
         (yield val)
         (traverse right)]))))
