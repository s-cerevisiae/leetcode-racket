#lang racket/base

(require racket/list
         racket/vector)

(define (subsets lst)
  (define v (list->vector lst))
  (define len (vector-length v))
  (for/list ([n (in-range (expt 2 len))])
    (for/list ([i (in-range (integer-length n))]
               #:when (bitwise-bit-set? n i))
      (vector-ref v i))))

(define (subsets-with-dup lst)
  (define v (list->vector lst))
  (vector-sort! v <)
  (define len (vector-length v))
  (define (duplicate? n i)
    (and (> i 0)
         (= (vector-ref v (- i 1)) (vector-ref v i))
         (not (bitwise-bit-set? n (- i 1)))))
  (define (select-by-num n)
    (let/cc return
      (for/list ([i (in-range (integer-length n))]
                 #:when (bitwise-bit-set? n i))
        (when (duplicate? n i) (return #f))
        (vector-ref v i))))
  (filter-map
    select-by-num
    (range (expt 2 len))))