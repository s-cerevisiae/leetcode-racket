#lang racket

(struct list-node (val next) #:transparent)
(define (make-list-node [val 0])
  (list-node val #f))

(define list? (or/c list-node? #f))

(define/contract (merge-two-lists l1 l2)
  (-> list? list? list?)
  (match* (l1 l2)
    [(#f #f) #f]
    [(xs #f) xs]
    [(#f ys) ys]
    [((list-node x xs) (list-node y ys))
     (if (< x y)
         (list-node x (merge-two-lists xs l2))
         (list-node y (merge-two-lists l1 ys)))]))
