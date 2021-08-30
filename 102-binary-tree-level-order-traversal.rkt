#lang racket

(require "tree-node.rkt")

(define (level-order root)
  (let traverse ([node root])
    (match node
      [#f '()]
      [(tree-node val left right)
       (cons (list val)
             (merge
               (traverse left)
               (traverse right)))])))

(define (merge ls rs)
  (let loop ([ls ls] [rs rs])
    (match* (ls rs)
      [(ls '()) ls]
      [('() rs) rs]
      [((cons l ls) (cons r rs))
       (cons (append l r) (loop ls rs))])))
