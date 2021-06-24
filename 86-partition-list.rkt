#lang racket

(require "list-node.rkt")

(require (only-in racket/control
                  shift reset))

(define (partition head x)
  (reset
    (let loop ([node head])
      (match node
        [#f #f]
        [(list-node val next)
         (if (< val x)
             (begin (shift k (list-node val (k (void))))
                    (loop next))
             (list-node val (loop next)))]))))
