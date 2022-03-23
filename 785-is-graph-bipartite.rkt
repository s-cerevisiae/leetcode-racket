#lang racket

(define (is-bipartite graph)
  (define gv (list->vector graph))
  (define len (vector-length gv))

  (define tags (make-hasheq))

  (for/and ([node (in-range len)])
    (define to-tag (hash-ref tags node 1))
    (let loop ([node node] [to-tag to-tag])
      (if (hash-has-key? tags node)
          (= (hash-ref tags node) to-tag)
          (begin
            (hash-set! tags node to-tag)
            (for/and ([nb (vector-ref gv node)])
              (loop nb (- to-tag))))))))
