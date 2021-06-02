#lang racket

(require data/gvector data/queue)

(struct tree-node
  (val left right) #:mutable #:transparent)

(define (make-tree-node [val 0])
  (tree-node val #f #f))

(define (zigzag-level-order root)
  (define result (gvector))
  (let traverse ([node root] [depth 0])
    (when node
      (unless (< depth (gvector-count result))
        (gvector-add! result (make-queue)))
      (match-define (tree-node val left right) node)
      ((if (even? depth) enqueue! enqueue-front!)
       (gvector-ref result depth)
       val)
      (traverse left (add1 depth))
      (traverse right (add1 depth))))
  (map queue->list (gvector->list result)))
