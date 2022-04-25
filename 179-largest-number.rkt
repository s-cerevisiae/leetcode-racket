#lang racket

(define (e x) (expt 10 x))

(define (count-digit n)
  (let loop ([n n] [d 1])
    (if (< n 10)
        d
        (loop (quotient n 10) (add1 d)))))

(define (first-digit n)
  (quotient n (e (- (count-digit n) 1))))

(define (combine x y)
  (+ (* x (e (count-digit y)))
     y))

(define (largest-number nums)
  (define (greater-than? x y)
    (let ([x-head (first-digit x)]
          [y-head (first-digit y)])
      (or (> x-head y-head)
          (and (= x-head y-head)
               (> (combine x y) (combine y x))))))
  (define lst (sort nums greater-than?))
  (if (zero? (car lst))
      "0"
      (string-append* (map number->string lst))))
