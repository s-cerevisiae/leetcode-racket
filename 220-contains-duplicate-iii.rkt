#lang racket

(require racket/stxparam syntax/parse/define)

(define-syntax-parameter it (syntax-rules ()))
(define-syntax-parse-rule (aand x:expr xs:expr ...)
  (let ([v x])
    (syntax-parameterize ([it (syntax-id-rules () [_ v])])
      (and v xs ...))))

(define (contains-nearby-almost-duplicate nums k t)
  (define v (list->vector nums))
  (define table (make-hash))

  (define (id x)
    (if (>= x 0)
        (quotient x (+ t 1))
        (- (quotient (+ x 1)
                     (+ t 1))
           1)))

  (define (near? x y)
    (<= (abs (- x y)) t))

  (for/or ([x (in-vector v)]
           [i (in-naturals)])
    (define id/x (id x))
    (begin0
      (or (hash-has-key? table id/x)
          (aand (hash-ref table (sub1 id/x) #f) (near? x it))
          (aand (hash-ref table (add1 id/x) #f) (near? x it)))
      (hash-set! table id/x x)
      (when (>= i k)
        (hash-remove! table (id (vector-ref v (- i k))))))))
