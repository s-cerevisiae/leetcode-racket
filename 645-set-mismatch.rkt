#lang racket

(define (find-error-nums nums)
  (let* ([n (length nums)]
         [sum/all (/ (* n (+ n 1)) 2)]
         [sum/nums (apply + nums)]
         [sum/dedup (for/sum ([x (in-set (list->set nums))]) x)])
    (list (- sum/nums sum/dedup) (- sum/all sum/dedup))))
