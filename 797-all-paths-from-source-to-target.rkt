#lang racket

(define (all-paths-source-target graph)
  (define gv (list->vector graph))
  (let search ([node 0])
    (define succ (vector-ref gv node))
    (cond [(= node (- (vector-length gv) 1))
           (list (list node))]
          [(null? succ) '()]
          [else
           (for*/list ([s succ]
                       [p (search s)])
             (cons node p))])))
