#lang racket/base

(provide (struct-out list-node)
         make-list-node)

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

(module* util #f
  (require racket/function
           racket/match
           racket/stream)

  (provide (all-defined-out))

  (define list->list-node
    (curry foldr list-node #f))

  (define (list-node-map p l)
    (list-node-foldr
      (λ (x r) (list-node (p x) r))
      #f l))

  (define (list-node-filter p l)
    (list-node-foldr
      (λ (x r) (if (p x) (list-node x r) r))
      #f l))

  (define (list-node-reverse l)
    (list-node-foldl list-node #f l))

  (define (list-node-append l1 l2)
    (list-node-foldr list-node l2 l1))

  (define (list-node-foldr p i l)
    (let loop ([l l])
      (match l
        [#f i]
        [(list-node val next)
         (p val (loop next))])))

  (define (list-node-foldl p i l)
    (let loop ([l l] [r i])
      (match l
        [#f r]
        [(list-node val next)
         (loop next (p val r))])))

  (define (in-list-node l)
    (match l
      [#f empty-stream]
      [(list-node val next)
       (stream-cons
         val
         (in-list-node next))])))
