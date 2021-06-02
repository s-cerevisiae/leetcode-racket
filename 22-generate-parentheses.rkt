#lang racket

(define (generate n)
  (if (zero? n)
      '("")
      (for*/list ([i (in-range n)]
                  [l (generate i)]
                  [r (generate (- n 1 i))])
        (string-append "(" l ")" r))))

(define (memoize f)
  (define memo (make-hash))
  (λ (x) (hash-ref! memo x
           (λ () (f x)))))

(set! generate (memoize generate))

(define generate-parenthesis
  generate)
