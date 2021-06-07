#lang racket

(define (find-median-sorted-arrays nums1 nums2)
  (define len (+ (length nums1) (length nums2)))
  (define mid (quotient len 2))
  (define merged
    (let loop ([l1 nums1] [l2 nums2] [merged '()] [i 0])
      (if (> i mid)
        merged
        (match* (l1 l2)
          [('() '()) merged]
          [((cons x xs) '()) (loop xs '() (cons x merged) (add1 i))]
          [('() (cons y ys)) (loop '() ys (cons y merged) (add1 i))]
          [((cons x xs) (cons y ys))
           (if (< x y)
             (loop xs l2 (cons x merged) (add1 i))
             (loop l1 ys (cons y merged) (add1 i)))]))))
  (match merged
    ['() #f]
    [(list* x _) #:when (odd? len)
     x]
    [(list* x y _)
     (/ (+ x y) 2)]))
