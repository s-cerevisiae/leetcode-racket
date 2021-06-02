#lang racket

(define (scanl f i l)
  (let loop ([acc i] [l l])
    (if (null? l)
      (list acc)
      (cons acc (loop (f acc (car l)) (cdr l))))))

(define (max-uncrossed-lines nums1 nums2)
  (for/fold ([last-line (make-list (add1 (length nums2)) 0)]
             #:result (last last-line))
            ([n1 (in-list nums1)])
    (scanl max 0
      (for/list ([n2 (in-list nums2)]
                 [d1 (in-list last-line)]
                 [d2 (in-list (cdr last-line))])
        (if (= n1 n2) (add1 d1) d2)))))
