#lang racket

(struct node (end? children) #:transparent #:mutable)

(define (make-leaf) (node #f (make-hash)))

(define trie%
  (class object%
    (super-new)
    (define root (make-leaf))

    (define/public (insert word)
      (define last-node
        (for/fold ([node root])
                  ([c (in-string word)])
          (hash-ref! (node-children node)
                     c
                     make-leaf)))
      (set-node-end?! last-node #t))

    (define/public (search word)
      (for/fold ([node root]
                 #:result (and node (node-end? node)))
                ([c (in-string word)])
        #:break (not node)
        (hash-ref (node-children node) c #f)))

    (define/public (startsWith prefix)
      (for/fold ([node root]
                 #:result (and node #t))
                ([c (in-string prefix)])
        #:break (not node)
        (hash-ref (node-children node) c #f)))))
