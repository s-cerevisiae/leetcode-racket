#lang racket

(define (top-k-frequent nums k)
  (let* ([counts (foldl (λ (x c) (hash-update c x add1 0))
                        (hash) nums)]
         [pairs (sequence->stream (in-hash-pairs counts))]
         [pairs-sorted (stream-sort cmp pairs)] ;(λ (x y) (> (cdr x) (cdr y))) pairs)]
         [nums-sorted (stream-map car pairs-sorted)]
         [top-k (stream-take nums-sorted k)])
    (stream->list top-k)))

(define (stream-merge lt? s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (let ([x (stream-first s1)]
               [y (stream-first s2)])
           (if (lt? x y)
               (stream-cons x (stream-merge lt? (stream-rest s1) s2))
               (stream-cons y (stream-merge lt? s1 (stream-rest s2)))))]))

(define (stream-sort lt? s)
  (let loop ([s s] [l (stream-length s)])
    (define n (quotient l 2))
    (if (< l 2)
        s
        (stream-merge
          lt?
          (loop (stream-take s n) n)
          (loop (stream-tail s n) n)))))
