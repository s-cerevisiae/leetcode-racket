#lang racket

(require "tree-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (flatten root)
  (define nodes
    (reset
      (let traverse ([node root])
        (match node
          [#f '()]
          [(tree-node _ left right)
           (shift k (cons node (k (void))))
           (traverse left)
           (traverse right)]))))
  (foldr connect! #f nodes)
  root)

(define (connect! node node*)
  (set-tree-node-left! node #f)
  (set-tree-node-right! node node*)
  node)
