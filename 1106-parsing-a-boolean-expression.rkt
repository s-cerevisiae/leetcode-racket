#lang racket

(define (parse-bool-expr expression)
  (define-values (result _)
    (parse (string->list expression)))
  result)

(define (parse l)
  (define (parse-args l)
    (let loop ([l l])
      (match l
        [(cons #\, cs) (loop cs)]
        [(cons #\) _) (values '() l)]
        [_ (let*-values ([(arg rest) (parse l)]
                         [(args rest) (loop rest)])
             (values (cons arg args) rest))])))

  (match l
    [(cons #\t cs) (values #t cs)]
    [(cons #\f cs) (values #f cs)]
    [(cons #\! cs)
     (define-values (arg rest) (parse (cdr cs)))
     (values (not arg) (cdr rest))]
    [(cons #\& cs)
     (define-values (args rest) (parse-args (cdr cs)))
     (values (andmap identity args) (cdr rest))]
    [(cons #\| cs)
     (define-values (args rest) (parse-args (cdr cs)))
     (values (ormap identity args) (cdr rest))]))
