#lang racket

(provide (struct-out tree-node)
         make-tree-node
         tree->list
         list->tree)

; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define/match (tree->list node)
  [(#f) '()]
  [((tree-node val #f #f))
   val]
  [((tree-node val left right))
   (list val (tree->list left) (tree->list right))])

(define/match (list->tree node)
  [('()) #f]
  [((list val left right))
   (tree-node val (list->tree left) (list->tree right))]
  [(val) (make-tree-node val)])
