#lang racket

(define stream-zip
  (case-lambda
    [(f xs ys)
     (for/stream ([x xs] [y ys])
       (f x y))]
    [(f xs . ys*)
     (foldl (curry stream-zip f)
            xs ys*)]))

(define trib-seq
  (stream* 0 1 1
           (stream-zip +
             trib-seq
             (stream-rest trib-seq)
             (stream-tail trib-seq 2))))

(define (tribonacci n)
  (stream-ref trib-seq n))
