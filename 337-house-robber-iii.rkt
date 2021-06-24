#lang racket

(require "tree-node.rkt")

(define/match (to-rob node)
  [(#f) 0]
  [((tree-node val left right))
   (+ val (not-to-rob left) (not-to-rob right))])

(define/match (not-to-rob node)
  [(#f) 0]
  [((tree-node val left right))
   (+ (rob left) (rob right))])

(define (rob node)
  (max (to-rob node) (not-to-rob node)))

(define (memoize f)
  (define memo (make-hasheq))
  (λ (x) (hash-ref! memo x
           (λ () (f x)))))

(set! rob (memoize rob))
