#lang racket

(define (min-swaps-couples row)
  (define graph
    (let loop ([row row]
               [graph (make-graph)])
      (match row
        [(list* l r rst)
         (define m (quotient l 2))
         (define n (quotient r 2))
         (loop rst
           (if (= m n)
               graph
               (graph-insert graph m n)))]
        [_ graph])))

  (define (search g q r)
    (if (queue-empty? q)
        (values g q r)
        (let*-values ([(n q) (dequeue q)]
                      [(ns g) (graph-remove g n)])
          (if (null? ns)
              (search g q r)
              (search g (enqueue* q ns) (+ r 1))))))

  (for/fold ([g graph]
             [q (make-queue)]
             [r 0]
             #:result r)
            ([n (in-set (graph-nodes graph))])
    (search g (enqueue q n) r)))

(define adj hasheq)
(define (adj-add adj n m)
  (~> adj
      (hash-update n (curry cons m) '())
      (hash-update m (curry cons n) '())))
(define (adj-remove adj n)
  (foldl
    (λ (m acc)
       (hash-update acc m (curry remove n)))
    (hash-remove adj n)
    (hash-ref adj n '())))

(struct graph [nodes edges] #:transparent)

(define (make-graph)
  (graph (seteq) (adj)))

(define (graph-empty? gr)
  (set-empty? (graph-nodes gr)))

(define (graph-insert gr n m)
  (match-define (graph nodes edges) gr)
  (graph 
    (~> nodes (set-add n) (set-add m))
    (adj-add edges n m)))

(define (graph-insert* gr edge*)
  (foldl
    (λ (e g) (graph-insert g e))
    gr edge*))

(define (graph-remove gr node)
  (match-define (graph nodes edges) gr)
  (values
    (hash-ref edges node '())
    (graph
      (set-remove nodes node)
      (adj-remove edges node))))

(struct queue [in out] #:transparent)

(define (make-queue . in) (queue in '()))

(define (queue-empty? q)
  (and (null? (queue-in q))
       (null? (queue-out q))))

(define (enqueue q v)
  (queue (cons v (queue-in q))
         (queue-out q)))

(define (enqueue* q v*)
  (queue (append v* (queue-in q))
         (queue-out q)))

(define (dequeue q)
  (match q
    [(queue '() '())
     (error "dequeue on empty queue")]
    [(queue in '())
     (dequeue (queue '() (reverse in)))]
    [(queue in (cons o os))
     (values o (queue in os))]))

(define-syntax ~>
  (syntax-rules ()
    [(_ expr) expr]
    [(_ expr (fn args ...) rest ...)
     (~> (fn expr args ...) rest ...)]
    [(_ expr fn rest ...)
     (~> (fn expr) rest ...)]))
