#lang racket

(define (candy ratings)
  (define rat (list->vector ratings))
  (define len (vector-length rat))
  (define (rating i) (vector-ref rat i))
  (define (left i)
    (and (> i 0) (- i 1)))
  (define (right i)
    (and (< i (- len 1))
         (+ i 1)))

  (define (candy i)
    (force (vector-ref candies i)))
  (define (calc-candy r i)
    (if (and i (> r (rating i)))
        (+ 1 (candy i))
        1))
  (define candies
    (for/vector ([i (in-range len)])
      (delay
        (let ([lk (left i)]
              [rk (right i)]
              [r (rating i)])
          (max (calc-candy r lk)
               (calc-candy r rk))))))
  (for/sum ([x (in-vector candies)])
    (force x)))
