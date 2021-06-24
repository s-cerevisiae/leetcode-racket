#lang racket

(define/contract (clumsy n)
  (-> exact-integer? exact-integer?)
  (for/fold ([stack (list n)]
             #:result (apply + stack))
            ([x (in-range (- n 1) 0 -1)]
             [op (in-cycle '(* / + -))])
    (match* (op stack)
      [('* (cons y ys))
       (cons (* y x) ys)]
      [('/ (cons y ys))
       (cons (quotient y x) ys)]
      [('+ ys) (cons x ys)]
      [('- ys) (cons (- x) ys)])))

(define (clumsy2 n)
  (match* (n (remainder n 4))
    [((or 1 2) _) n]
    [(3 _) 6]
    [(4 _) 7]
    [(_ 0) (+ n 1)]
    [(_ (or 1 2)) (+ n 2)]
    [(_ 3) (- n 1)]))
