#lang racket

(require "list-node.rkt")

(define (sort-list head)
  (define len (list-length head))
  (if (< len 2)
      head
      (let-values ([(l r) (list-split-at head (quotient len 2))])
        (merge (sort-list l) (sort-list r)))))

(define/match (merge l1 l2)
  [(l1 #f) l1]
  [(#f l2) l2]
  [((list-node v1 n1) (list-node v2 n2))
   (if (< v1 v2)
       (list-node v1 (merge n1 l2))
       (list-node v2 (merge l1 n2)))])

(define (list-split-at l n)
  (let loop ([l l] [n n] [pfx #f])
    (if (zero? n)
        (values (list-reverse pfx) l)
        (match l
          [#f (error "invalid index")]
          [(list-node val next)
           (loop next (sub1 n) (list-node val pfx))]))))

(define (list-foldl p i l)
  (let loop ([node l] [r i])
    (match node
      [#f r]
      [(list-node val next)
       (loop next (p val r))])))

(define list-reverse
  (curry list-foldl list-node #f))

(define list-length
  (curry list-foldl (λ (_ n) (add1 n)) 0))
