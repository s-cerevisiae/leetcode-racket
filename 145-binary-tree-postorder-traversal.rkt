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

(define (postorder go value left right)
  (go left)
  (go right)
  (yield value))

(define postorder-traversal
  (curry traverse '() postorder))
