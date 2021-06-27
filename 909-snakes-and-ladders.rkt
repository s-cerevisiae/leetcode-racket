#lang racket

(define (board->vector board)
  (for*/vector ([(row n) (in-indexed (in-list (reverse board)))]
                [x (in-list (if (odd? n) (reverse row) row))])
    x))

(define (snakes-and-ladders board)
  (define v (board->vector board))
  (define dest (vector-length v))

  (define (teleport x)
    (define d (vector-ref v (sub1 x)))
    (if (= d -1) x d))
  (define (move x)
    (map teleport
      (filter (curry >= dest)
        (range (+ x 1) (+ x 7)))))

  (let search ([states '((1 . 0))] [seen (set)])
    (match states
      ['() -1]
      [(cons (cons x step) rst)
       (cond [(set-member? seen x) (search rst seen)]
             [(= dest x) step]
             [else
              (search (append rst (map (curryr cons (add1 step))
                                       (move x)))
                      (set-add seen x))])])))
