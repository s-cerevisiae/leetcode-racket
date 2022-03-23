#lang racket

(define (find-order course-num prerequisites)
  (define nodes (range course-num))
  (define graph
    (graph-insert* (make-graph nodes) prerequisites))
  (define to-visit
    (enqueue*
      (make-queue)
      (filter (curry is-source? graph) nodes)))
  (toposort graph to-visit))

(define (toposort init-gr to-visit)
  (let loop ([gr init-gr]
             [to-visit to-visit]
             [result '()])
    (if (queue-empty? to-visit)
        (if (graph-empty? gr)
            (reverse result)
            '())
        (let*-values
          ([(node to-visit) (dequeue to-visit)]
           [(_ succs rest-gr) (graph-remove gr node)])
          (define sources
            (filter (curry is-source? rest-gr) succs))
          (loop
            rest-gr
            (enqueue* to-visit sources)
            (cons node result))))))

(define adj hasheq)
(define (adj-add adj n m)
  (hash-update adj n (curry cons m) '()))
(define (adj-remove adj node targets)
  (for/fold ([adj adj])
            ([t (in-list targets)])
    (hash-update
      adj t
      (λ (l) (remove node l)))))

(struct graph [nodes ins outs]
  #:transparent)

(define (make-graph nodes)
  (graph (list->seteq nodes) (adj) (adj)))

(define (graph-empty? gr)
  (set-empty? (graph-nodes gr)))

(define (graph-insert gr edge)
  (match-let ([(graph nodes ins outs) gr]
              [(list to from) edge])
    (graph nodes
           (adj-add ins to from)
           (adj-add outs from to))))

(define (graph-insert* gr edge*)
  (foldl
    (λ (e g) (graph-insert g e))
    gr edge*))

(define (graph-remove gr node)
  (match-define (graph nodes ins outs) gr)
  (let ([node-ins (hash-ref ins node '())]
        [node-outs (hash-ref outs node '())])
    (values
      node-ins
      node-outs
      (graph
        (set-remove nodes node)
        (adj-remove ins node node-outs)
        (adj-remove outs node node-ins)))))

(define (is-source? gr node)
  (null? (hash-ref (graph-ins gr) node '())))

(struct queue [in out]
  #:transparent)

(define (make-queue) (queue '() '()))

(define (queue-empty? q)
  (and (null? (queue-in q))
       (null? (queue-out q))))

(define (enqueue q v)
  (queue (cons v (queue-in q))
         (queue-out q)))

(define (enqueue* q v*)
  (foldl (λ (v q) (enqueue q v))
         q v*))

(define (dequeue q)
  (match q
    [(queue '() '())
     (error "dequeue on empty queue")]
    [(queue in '())
     (dequeue (queue '() (reverse in)))]
    [(queue in (cons o os))
     (values o (queue in os))]))
