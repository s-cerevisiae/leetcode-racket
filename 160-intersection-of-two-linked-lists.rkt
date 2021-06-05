#lang racket

(require "list-node.rkt")

(module+ test
  (define (list->list-node lst)
    (foldr list-node #f lst))

  (define (list-node-append head-a head-b)
    (let loop ([node head-a])
      (if (not (list-node-next node))
        (set-list-node-next! node head-b)
        (loop (list-node-next node))))
    head-a))

(define (get-intersection-node head-a head-b)
  (define nodes-a
    (let loop ([node head-a] [nodes (seteq)])
      (if (not node)
        nodes
        (loop (list-node-next node) (set-add nodes node)))))
  (let loop ([node head-b])
    (cond [(not node) #f]
          [(set-member? nodes-a node) node]
          [else (loop (list-node-next node))])))

(module+ test
  (require rackunit)

  (test-case "Example A"
    (define tail1 (list->list-node '(8 4 5)))
    (define head1-a (list-node-append (list->list-node '(4 1))
                                      tail1))
    (define head1-b (list-node-append (list->list-node '(5 0 1))
                                      tail1))
    (check-equal? (get-intersection-node head1-a head1-b)
                  tail1))
  
  (test-case "Example B"
    (define tail2 (list->list-node '(2 4)))
    (define head2-a (list-node-append (list->list-node '(0 9 1))
                                      tail2))
    (define head2-b (list-node-append (make-list-node 3)
                                      tail2))
    (check-equal? (get-intersection-node head2-a head2-b)
                  tail2))

  (test-case "Example C"
    (define head3-a (list->list-node '(2 6 4)))
    (define head3-b (list->list-node '(1 5)))
    (check-equal? (get-intersection-node head3-a head3-b)
                  #f)))
