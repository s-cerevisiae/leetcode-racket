#lang racket

(require "tree-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (traverse init proc tree)
  (reset
    (let go ([node tree])
      (match node
        [#f init]
        [(tree-node value left right)
         (proc go value left right)
         init]))))

(define (yield value)
  (shift k (cons value (k))))

(define (preorder go value left right)
  (yield value)
  (go left)
  (go right))

(define preorder-traversal
  (curry traverse '() preorder))
