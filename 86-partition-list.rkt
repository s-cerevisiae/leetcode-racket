#lang racket

;; simple showcase of delimited control operators, shift and reset
;; http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf

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
