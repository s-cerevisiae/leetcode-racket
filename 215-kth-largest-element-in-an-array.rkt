#lang racket

(define (find-kth-largest nums k)
  (define pq
    (foldl (λ (x pq) (pq-push pq x))
           (priority-queue)
           nums))
  (let loop ([k k] [pq pq])
    (if (= k 1)
        (pq-peek pq)
        (loop (- k 1) (pq-pop pq)))))

;; Binomial queue as in Optimal Purely Functional Priority Queues.
;; This implementation isn't quite optimal though.
(define (priority-queue) '())

(define (link t1 t2)
  (match* (t1 t2)
    [('() t) t]
    [(t '()) t]
    [((cons r1 c1) (cons r2 c2))
     (if (> r1 r2)
         (cons r1 (cons t2 c1))
         (cons r2 (cons t1 c2)))]))

(define (merge h1 h2)
  (let loop ([carry '()] [h1 h1] [h2 h2])
    (match (cons h1 h2)
      [(or (cons '() h) (cons h '()))
       (if (null? carry)
           h
           (loop '() (list carry) h))]
      [(cons (cons t1 r1) (cons t2 r2))
       (if (or (null? t1) (null? t2))
           (cons (link carry (link t1 t2))
                 (loop '() r1 r2))
           (cons carry
                 (loop (link t1 t2) r1 r2)))])))

(define (pq-push pq x)
  (merge (list (list x)) pq))

(define (max-tree pq)
  (let loop ([pq pq] [target #f])
    (match pq
      [(cons (and t (cons x _)) r)
       (if (or (not target)
               (> x (car target)))
           (loop r t)
           (loop r target))]
      [(cons '() r) (loop r target)]
      ['() target])))

(define (pq-peek pq)
  (define t (max-tree pq))
  (and t (car t)))

(define (remove-tree t h)
  (let loop ([h h])
    (cond [(null? h) '()]
          [(eq? t (car h))
           (if (null? (cdr h))
               '()
               (cons '() (loop (cdr h))))]
          [else (cons (car h) (loop (cdr h)))])))

(define (pq-pop pq)
  (define t (max-tree pq))
  (if t
      (merge (reverse (cdr t))
             (remove-tree t pq))
      pq))
