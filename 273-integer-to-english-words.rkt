#lang racket

(define (make-num-table ks vs)
  (for/hash ([k (in-list ks)]
             [v (in-list vs)])
    (values k v)))

(define ones
  (make-num-table (range 10)
    '(() "One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine")))
(define teens
  (make-num-table (range 10 20)
    '("Ten" "Eleven" "Twelve" "Thirteen" "Fourteen" "Fifteen" "Sixteen" "Seventeen" "Eighteen" "Nineteen")))
(define tens
  (make-num-table (range 2 10)
    '("Twenty" "Thirty" "Forty" "Fifty" "Sixty" "Seventy" "Eighty" "Ninety")))

(define (3digit->words n)
  (define-values (h to)
    (quotient/remainder n 100))
  (define-values (t o)
    (quotient/remainder to 10))
  (define (__d)
    (hash-ref ones o))
  (define (_d_)
    (hash-ref tens t))
  (define (_dd)
    (cond [(< to 10) (__d)]
          [(< to 20) (hash-ref teens to)]
          [else (list (_d_) (__d))]))
  (define (d__)
    (list (hash-ref ones h)
          "Hundred"))
  (define (ddd)
    (if (zero? h)
      (_dd)
      (list (d__) (_dd))))
  (flatten (ddd)))

(define (number->3digits n)
  (if (zero? n)
    '()
    (let-values ([(quo rem) (quotient/remainder n 1000)])
      (cons rem (number->3digits quo)))))

(define (number->words/nonzero num)
  (define separated-words
    (for/list ([ds (in-list (number->3digits num))]
               [sep (in-list '(() "Thousand" "Million" "Billion"))]
               #:when (not (zero? ds)))
      (list (3digit->words ds) sep)))
  (string-join (flatten (reverse separated-words))))

(define (number-to-words num)
  (if (zero? num)
    "Zero"
    (number->words/nonzero num)))
