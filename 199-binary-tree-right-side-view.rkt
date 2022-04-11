#lang racket

(require "tree-node.rkt")

(define (right-side-view root)
  (let traverse ([node root])
    (match node
      [(tree-node val left right)
       (define lv (traverse left))
       (define rv (traverse right))
       (cons val (replace/right lv rv))]
      [#f '()])))

(define (replace/right l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (cons (car l2)
                    (replace/right (cdr l1) (cdr l2)))]))
