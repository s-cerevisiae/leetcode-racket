#lang racket

(require data/heap)

(define (swim-in-water grid)
  (define v (list->vector (map list->vector grid)))
  (define n (vector-length v))
  (define end (cons (sub1 n) (sub1 n)))

  (define (height pos)
    (vector-ref (vector-ref v (cdr pos))
                (car pos)))

  (define (neighbors pos)
    (match-define (cons x y) pos)
    (filter (match-lambda
              [(cons x y) (and (<= 0 x (sub1 n))
                               (<= 0 y (sub1 n)))])
            (list (cons (- x 1) y)
                  (cons x (- y 1))
                  (cons (+ x 1) y)
                  (cons x (+ y 1)))))

  (define visited (mutable-set))
  (define to-visit (make-heap (λ (p q) (<= (height p) (height q)))))
  (heap-add! to-visit '(0 . 0))

  (let loop ([result 0])
    (define pos (heap-min to-visit))
    (heap-remove-min! to-visit)
    (cond [(equal? end pos)
           (max result (height end))]
          [(set-member? visited pos)
           (loop result)]
          [else
           (set-add! visited pos)
           (heap-add-all! to-visit (neighbors pos))
           (loop (max result (height pos)))])))
