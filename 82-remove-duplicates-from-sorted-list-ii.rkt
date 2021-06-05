#lang racket

(require "list-node.rkt")

(define (delete-duplicates head)
  (let loop ([last-val #f] [node head] [cont #f])
    (match node
      [#f #f]
      [(list-node (== last-val) next)
       (cont (loop last-val next cont))]
      [(list-node val next)
       (let/cc cc (list-node val (loop val next cc)))])))
