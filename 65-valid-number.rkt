#lang racket

(define ((then f g) input cont)
  (f input (λ (rst) (g rst cont))))

(define ((alt f g) input cont)
  (or (f input cont) (g input cont)))

(define ((opt f) input cont)
  (or (f input cont)
      (cont input)))

(define ((many f) input cont)
  (or ((then f (many f)) input cont)
      (cont input)))

(define (many1 f)
  (then f (many f)))

(define (end input [cont #t])
  (null? input))

(define ((satisfy f) input cont)
  (and (pair? input)
       (f (car input))
       (cont (cdr input))))

(define (is c)
  (satisfy (λ (v) (char=? v c))))

(define (is/ci c)
  (satisfy (λ (v) (char-ci=? v c))))

; ------------------------------------

(define digit (satisfy char-numeric?))

(define dot (is #\.))

(define sign (alt (is #\+) (is #\-)))

(define integer
  (then (opt sign)
        (many1 digit)))

(define lead-dot
  (then dot (many1 digit)))

(define lead-digit
  (then (many1 digit)
        (then dot (many digit))))

(define decimal
  (then (opt sign)
        (alt lead-dot lead-digit)))

(define exponent
  (then (is/ci #\e) integer))

(define number
  (then (alt integer decimal)
        (opt exponent)))

(define (is-number s)
  (number (string->list s) end))
