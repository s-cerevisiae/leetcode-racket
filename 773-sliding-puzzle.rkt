#lang racket

(require data/queue)

(define (sliding-puzzle board)
  (define visited (mutable-set))
  (define visited? (curry set-member? visited))
  (define solved? (curry equal? #(1 2 3 4 5 0)))

  (define initial (list->vector (flatten board)))
  (define states (make-queue))
  (enqueue! states (cons initial 0))

  (let search ()
    (if (queue-empty? states)
        -1
        (match-let ([(cons state depth) (dequeue! states)])
          (cond [(visited? state) (search)]
                [(solved? state) depth]
                [else
                 (set-add! visited state)
                 (for ([next (in-list (slide state))])
                   (enqueue! states (cons next (add1 depth))))
                 (search)])))))

(define neighbors
  #((1 3) (0 2 4) (1 5) (0 4) (1 3 5) (2 4)))

(define (slide state)
  (define x (index-of (vector->list state) 0 =))
  (for/list ([y (in-list (vector-ref neighbors x))])
    (vector-swap state x y)))

(define (vector-swap v x y)
  (let ([x-val (vector-ref v x)]
        [y-val (vector-ref v y)]
        [new-v (vector-copy v)])
    (vector-set*! new-v x y-val y x-val)
    new-v))
