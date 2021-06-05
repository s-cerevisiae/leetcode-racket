#lang racket

(require "list-node.rkt")

(define/match (swap-pairs head)
  [((or #f (list-node _ #f))) head]
  [((list-node x (list-node y next)))
   (list-node y (list-node x (swap-pairs next)))])
