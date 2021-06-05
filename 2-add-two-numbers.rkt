#lang racket

(require "list-node.rkt")

(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))
  (define (carry x) (quotient/remainder x 10))
  (let loop ([l1 l1] [l2 l2] [rem 0])
    (match* (l1 l2)
      [(#f #f) (if (zero? rem) #f (make-list-node rem))]
      [((list-node x xs) #f)
       (define-values (new-rem val) (carry (+ x rem)))
       (list-node val (loop xs #f new-rem))]
      [(#f (list-node y ys))
       (define-values (new-rem val) (carry (+ y rem)))
       (list-node val (loop #f ys new-rem))]
      [((list-node x xs) (list-node y ys))
       (define-values (new-rem val) (carry (+ x y rem)))
       (list-node val (loop xs ys new-rem))])))
