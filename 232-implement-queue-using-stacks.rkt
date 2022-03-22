#lang racket

(define my-queue%
  (class object%
    (super-new)
    (init-field)

    (define in '())
    (define out '())

    (define (ensure-has-out!)
      (when (null? out)
        (when (null? in)
          (error "empty queue"))
        (set! out (reverse in))
        (set! in '())))

    ; push : exact-integer? -> void?
    (define/public (push x)
      (set! in (cons x in)))

    ; pop : -> exact-integer?
    (define/public (pop)
      (ensure-has-out!)
      (let ([lst out])
        (set! out (cdr lst))
        (car lst)))

    ; peek : -> exact-integer?
    (define/public (peek)
      (ensure-has-out!)
      (car out))

    ; empty : -> boolean?
    (define/public (empty)
      (and (null? in)
           (null? out)))))
