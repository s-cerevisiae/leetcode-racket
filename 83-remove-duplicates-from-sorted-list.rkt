#lang racket

(require "list-node.rkt")

(define (delete-duplicates head)
  (let loop ([last-val #f] [node head])
    (match node
      [#f #f]
      [(list-node (== last-val) next)
       (loop last-val next)]
      [(list-node val next)
       (list-node val (loop val next))])))
