#lang racket

(require "tree-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (max-path-sum root)
  (reset
    (let traverse ([node root])
      (match node
        [#f 0]
        [(tree-node val left right)
         (let ([left-gain (max 0 (traverse left))]
               [right-gain (max 0 (traverse right))])
           (shift k (max (+ val left-gain right-gain) (k (void))))
           (+ val (max left-gain right-gain)))]))))
