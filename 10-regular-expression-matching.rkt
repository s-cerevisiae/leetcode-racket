#lang racket

(define (is-match s p)
  ((parse (string->list p))
   (string->list s)
   #f))

(define (parse pat)
  (define (char c)
    (if (char=? #\. c) any (is c)))
  (match pat
    [(list* c #\* rst)
     (then (many (char c)) (parse rst))]
    [(cons c rst)
     (then (char c) (parse rst))]
    ['() end]))

(define ((then f g) input cont)
  (f input (λ (rst) (g rst cont))))

(define (any input cont)
  (and (cons? input)
       (cont (cdr input))))

(define ((is c) input cont)
  (and (cons? input)
       (char=? c (car input))
       (cont (cdr input))))

(define ((many f) input cont)
  (or (f input (λ (rst) ((many f) rst cont)))
      (cont input)))

(define (end input cont)
  (null? input))
