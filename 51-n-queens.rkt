#lang racket

(define (queens n)
  (define (safe? x2 y2 lq)
    (for/and ([x1 (in-list lq)]
              [y1 (in-range (sub1 y2) -1 -1)])
      (not (or (= x1 x2)
               (= (abs (- x2 x1))
                  (- y2 y1))))))
  (let queen-cols ([k n])
    (if (= k 0)
        '(())
        (for*/list ([rq (in-list (queen-cols (sub1 k)))]
                    [r (in-range n)]
                    #:when (safe? r k rq))
          (cons r rq)))))

(define ((board->string n) b)
  (define (column q)
    (build-string n (λ (x) (if (= q x) #\Q #\.))))
  (foldl (λ (c l) (cons (column c) l)) '() b))

(define (solve-n-queens n)
  (map (board->string n) (queens n)))
