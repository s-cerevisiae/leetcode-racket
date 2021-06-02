#lang racket

(require syntax/parse)

(define (trans stx)
  (syntax-parse stx
    #:datum-literals ([plet let] add mult)
    [(add e1 e2)
     #`(+ #,(trans #'e1) #,(trans #'e2))]
    [(mult e1 e2)
     #`(* #,(trans #'e1) #,(trans #'e2))]
    [(plet (~seq name:id value) ... e)
     (let ([name* (syntax-e #'(name ...))]
           [value* (syntax-e #'(value ...))])
       #`(let* #,(for/list ([n (in-list name*)]
                            [v (in-list value*)])
                   (list n (trans v)))
           #,(trans #'e)))]
    [_ stx]))

(define base (make-base-namespace))

(define (evaluate expression)
  (eval (trans (call-with-input-string expression read))
        base))
