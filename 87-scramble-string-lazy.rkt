#lang racket/base

(module solution lazy
  (provide is-scramble)

  (define (matrix-ref dp . is)
    (foldl (λ (i v) (vector-ref v i)) dp is))

  (define (is-scramble s1 s2)
    (define n (! (string-length s1)))
    (define dp
      (for/vector ([p1 (in-range n)])
        (for/vector ([p2 (in-range n)])
          (for/vector ([k (in-range (! (add1 (- n (max p1 p2)))))])
            (cond [(= k 0) #t]
                  [(= k 1) (char=? (string-ref s1 p1)
                                   (string-ref s2 p2))]
                  [else
                    (ormap
                      (λ (sp)
                         (or (and (matrix-ref dp p1 p2 sp)
                                  (matrix-ref dp (+ p1 sp) (+ p2 sp) (- k sp)))
                             (and (matrix-ref dp p1 (+ p2 (- k sp)) sp)
                                  (matrix-ref dp (+ p1 sp) p2 (- k sp)))))
                      (range 1 k))])))))
    (! (matrix-ref dp 0 0 n))))

(require 'solution)
