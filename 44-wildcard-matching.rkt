#lang racket

(define (is-match s p)
  ((parse (string->list p))
   (string->list s)
   #f))

(define (parse pat)
  (foldr (λ (c rst) (then (char c) rst))
         end pat))

(define (char c)
  (match c
    [#\* (many any)]
    [#\? any]
    [c (is c)]))

(define ((then f g) input cont)
  (f input (λ (rst) (g rst cont))))

(define (any input cont)
  (and (cons? input)
       (cont (cdr input))))

(define ((is c) input cont)
  (and (cons? input)
       (char=? c (car input))
       (cont (cdr input))))

(define ((many f [memo (make-hasheq)]) input cont)
  (hash-ref! memo input
    (λ () (or ((then f (many f memo)) input cont)
              (cont input)))))

(define (end input cont)
  (null? input))
