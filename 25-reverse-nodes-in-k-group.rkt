#lang racket

(require "list-node.rkt")

(define (list-foldr f i l)
  (match l
    [#f i]
    [(list-node val next)
     (f val (list-foldr f i next))]))

(define (list-append . l)
  (foldr (λ (l1 l2) (list-foldr list-node l2 l1))
         #f l))

(define (list-reverse-split l k)
  (let loop ([left #f] [right l] [n k])
    (if (zero? n)
      (values left right)
      (match right
        [#f (values #f l)]
        [(list-node val next)
         (loop (list-node val left) next (sub1 n))]))))

(define (reverse-k-group head k)
  (apply list-append
    (let loop ([node head])
      (define-values (left right)
        (list-reverse-split node k))
      (if left
        (cons left (loop right))
        (list node)))))
