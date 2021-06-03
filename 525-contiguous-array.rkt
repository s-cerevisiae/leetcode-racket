#lang racket

(define (find-max-length nums)
  (define table (make-hash '((0 . -1))))
  (for/fold ([cnt 0] [len 0]
             #:result len)
            ([n (in-list nums)]
             [i (in-naturals)])
    (define next-cnt
      ((if (= n 1) add1 sub1) cnt))
    (define prev-i (hash-ref! table next-cnt i))
    (values next-cnt
            (max len (- i prev-i)))))
