#lang racket

(define (can-finish course-num prerequisites)
  (define graph (make-hasheq))
  (for ([p (in-list prerequisites)])
    (hash-update!
      graph
      (car p) (curry cons (cadr p))
      '()))

  (define tags (make-hasheq))
  (define (tag course)
    (hash-ref tags course 'untouched))
  (define (tag! t course)
    (hash-set! tags course t))

  (define (no-loops? course)
    (begin0
      (match (tag course)
        ['completed #t]
        ['in-progress #f]
        ['untouched
         (tag! 'in-progress course)
         (for/and ([c (hash-ref graph course '())])
           (no-loops? c))])
      (tag! 'completed course)))

  (for/and ([course (in-hash-keys graph)])
    (no-loops? course)))
