#lang racket

(define (distance start end)
  (match-let ([(list x0 y0) start]
              [(list x1 y1) end])
    (+ (abs (- x1 x0))
       (abs (- y1 y0)))))

(define (escape-ghosts ghosts target)
  (< (distance '(0 0) target)
     (apply min (map (λ (ghost) (distance ghost target)) ghosts))))
