#lang racket

(define (scanl f i l)
  (let loop ([acc i] [l l])
    (if (null? l)
      (list acc)
      (cons acc (loop (f acc (car l)) (cdr l))))))

(define/match (query v q)
  [(v (list l r))
   (bitwise-xor (vector-ref v (+ r 1))
                (vector-ref v l))])

(define (xor-queries arr queries)
  (define prefix (list->vector (scanl bitwise-xor 0 arr)))
  (map (λ (q) (query prefix q)) queries))
