#lang racket

(require "tree-node.rkt")

(require racket/control)

(define (record x)
  (shift k (cons x (k (void)))))

(define (start f . arg*)
  (reset (apply f arg*)))

(define (path-sum root target)
  (define (traverse node path sum)
    (match node
      [(tree-node val left right)
       (let ([path (cons val path)]
             [sum (+ val sum)])
         (when (and (nor left right)
                    (= sum target))
           (record (reverse path)))
         (traverse left path sum)
         (traverse right path sum))]
      [_ (void)])
    '())
  (start traverse root '() 0))
