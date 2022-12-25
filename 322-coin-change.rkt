#lang racket

(define (coin-change coins amount)
  (define memo (make-hasheq))
  (let loop ([amount amount])
    (cond [(zero? amount) 0]
          [(hash-has-key? memo amount)
           (hash-ref memo amount)]
          [else
           (define counts
             (for/list ([c (in-list coins)]
                        #:when (>= amount c)
                        [count (in-value (loop (- amount c)))]
                        #:when (>= count 0))
               count))
           (define count
             (if (null? counts)
                 -1
                 (+ 1 (apply min counts))))
           (hash-set! memo amount count)
           count])))
