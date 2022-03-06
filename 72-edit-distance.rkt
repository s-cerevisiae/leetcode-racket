#lang racket

(define (min-distance word1 word2)
  (define len1 (string-length word1))
  (define len2 (string-length word2))

  (define (dp-ref i j)
    (force (vector-ref (vector-ref dp i) j)))
  (define dp
    (for/vector ([i (in-range (+ len1 1))])
      (for/vector ([j (in-range (+ len2 1))])
        (cond [(= i 0) j]
              [(= j 0) i]
              [(char=? (string-ref word1 (- i 1))
                       (string-ref word2 (- j 1)))
               (delay (dp-ref (- i 1) (- j 1)))]
              [else
               (delay
                 (+ 1 (min (dp-ref i (- j 1))
                           (dp-ref (- i 1) j)
                           (dp-ref (- i 1) (- j 1)))))]))))

  (dp-ref len1 len2))
