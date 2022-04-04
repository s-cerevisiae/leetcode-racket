#lang racket

(define MIN -2147483648)
(define MAX 2147483647)

(define << arithmetic-shift)
(define (>>1 n) (<< n -1))
(define (<<1 n) (<< n 1))

(define (align x y)
  (let ([xl (integer-length x)]
        [yl (integer-length y)])
    (<< y (max 0 (- xl yl)))))

(define (divide-nat dividend divisor)
  (let loop ([x dividend]
             [y (align dividend divisor)]
             [r 0])
    (cond [(= y divisor)
           (if (< x y) r (+ r 1))]
          [(< x y) (loop x (>>1 y) (<<1 r))]
          [else (loop (- x y) y (+ r 1))])))

(define (divide dividend divisor)
  (if (and (= dividend MIN)
           (= divisor -1))
    MAX
    ((if (xor (< dividend 0)
              (< divisor 0))
         - +)
     ;; oops, overflow here!
     ;; should convert both to negative,
     ;; but I'm too lazy to pretend that we have 32bit integers
     (divide-nat (abs dividend) (abs divisor)))))
  
