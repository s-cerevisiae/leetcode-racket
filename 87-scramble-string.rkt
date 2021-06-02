#lang racket

(define (string-split-at str pos)
  (values
    (substring str 0 pos)
    (substring str pos)))

(define (is-scramble s1 s2)
  (or (string=? s1 s2)
      (let ([len (string-length s1)])
        (for/or ([i (in-range 1 len)])
          (let-values ([(sa sb) (string-split-at s1 i)]
                       [(sc sd) (string-split-at s2 i)]
                       [(se sf) (string-split-at s2 (- len i))])
            (or (and (is-scramble sa sc)
                     (is-scramble sb sd))
                (and (is-scramble sa sf)
                     (is-scramble sb se))))))))

(define (memoize f)
  (define cache (make-hash))
  (λ (s1 s2) (hash-ref! cache (cons s1 s2)
               (λ () (f s1 s2)))))

(set! is-scramble (memoize is-scramble))
