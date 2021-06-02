#lang racket

(module lexp racket/base
  (provide anchor)
  (require (for-syntax racket/base)
           syntax/parse/define)
  (define add +)
  (define mult *)
  (define-syntax-parse-rule
    (let (~seq name:id value) ... expr)
    #:with pairs (for/list ([n (in-list (syntax-e #'(name ...)))]
                            [v (in-list (syntax-e #'(value ...)))])
                   (list n v))
    (let* pairs expr))
  (define-namespace-anchor anchor))

(require 'lexp)

(define lexp (namespace-anchor->namespace anchor))

(define (evaluate expression)
  (eval (call-with-input-string expression read)
        lexp))
