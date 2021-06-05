#lang racket

(require "list-node.rkt")

(define (remove-elements head target)
  (match head
    [#f #f]
    [(list-node (? (curry = target)) next)
     (remove-elements next target)]
    [(list-node val next)
     (list-node val (remove-elements next target))]))
