#lang racket

(define (char->digit c)
  (- (char->integer c) 48))

(define (digit->char d)
  (integer->char (+ d 48)))

(define (in-digits s)
  (sequence-map char->digit (in-string s)))

(define (drop-leading-zeroes lst)
  (define l (dropf lst zero?))
  (if (null? l) '(0) l))

(define (vector-update! vec pos f)
  (vector-set! vec pos
    (f (vector-ref vec pos))))

(define (multiply num1 num2)
  (let* ([len (+ (string-length num1)
                 (string-length num2)
                 -1)]
         [ds (make-vector len)])
    (for* ([(x1 i1) (in-indexed (in-digits num1))]
           [(x2 i2) (in-indexed (in-digits num2))])
      (vector-update! ds (+ i1 i2) (curry + (* x1 x2))))

    (define result
      (for/fold ([carry 0] [digits '()]
                 #:result (drop-leading-zeroes
                            (cons carry digits)))
                ([x (in-vector ds (sub1 len) -1 -1)])
        (define-values (c d)
          (quotient/remainder (+ carry x) 10))
        (values c (cons d digits))))

    (list->string (map digit->char result))))
