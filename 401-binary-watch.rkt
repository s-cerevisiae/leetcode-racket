#lang racket

(define (bit-count n)
  (if (zero? n)
      0
      (add1 (bit-count (bitwise-and n (sub1 n))))))

(define (read-binary-watch turned-on)
  (for*/list ([h (in-range 12)]
              [m (in-range 60)]
              #:when (= turned-on (+ (bit-count h)
                                     (bit-count m))))
    (format "~a:~a"
            h
            (~r m #:min-width 2 #:pad-string "0"))))
