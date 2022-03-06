#lang racket

(define (matrix-ref dp . is)
  (foldl (λ (i v) (vector-ref v i)) dp is))

(define (is-scramble s1 s2)
  (define n (string-length s1))
  (define (dp-ref . is)
    (force (apply matrix-ref dp is)))
  (define dp
    (for/vector ([p1 (in-range n)])
      (for/vector ([p2 (in-range n)])
        (for/vector ([k (in-range (add1 (- n (max p1 p2))))])
          (cond [(= k 0) #t]
                [(= k 1)
                 (char=? (string-ref s1 p1)
                         (string-ref s2 p2))]
                [else
                 (delay
                   (for/or ([sp (in-range 1 k)])
                     (or (and (dp-ref p1 p2 sp)
                              (dp-ref (+ p1 sp) (+ p2 sp) (- k sp)))
                         (and (dp-ref p1 (+ p2 (- k sp)) sp)
                              (dp-ref (+ p1 sp) p2 (- k sp))))))])))))
  (dp-ref 0 0 n))
