#lang racket

(define (num-islands grid)
  (define v (list->vector (map list->vector grid)))
  (define h (vector-length v))
  (define w (if (zero? h) 0 (vector-length (vector-ref v 0))))

  (define (grid-ref x y) (vector-ref (vector-ref v y) x))

  (define (land? x y) (char=? #\1 (grid-ref x y)))

  (define (visited? x y) (char=? #\2 (grid-ref x y)))
  (define (visit! x y) (vector-set! (vector-ref v y) x #\2))

  (define (neighbors pos)
    (match-define (cons x y) pos)
    (filter (match-lambda
              [(cons x y) (and (<= 0 x (sub1 w))
                               (<= 0 y (sub1 h)))])
            (list (cons (- x 1) y)
                  (cons x (- y 1))
                  (cons (+ x 1) y)
                  (cons x (+ y 1)))))

  (for*/sum ([(row y0) (in-indexed (in-list grid))]
             [(i x0) (in-indexed (in-list row))])
    (let traverse ([p (cons x0 y0)])
      (match-define (cons x y) p)
      (cond [(visited? x y) 0]
            [(land? x y)
             (visit! x y)
             (map traverse (neighbors p))
             1]
            [else
             (visit! x y)
             0]))))
