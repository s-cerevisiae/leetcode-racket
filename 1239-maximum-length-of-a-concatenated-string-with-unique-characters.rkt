#lang racket

(define (charset-add st v)
  (let/cc fail
    (for/fold ([st st])
              ([c (in-string v)])
      (if (set-member? st c)
          (fail #f)
          (set-add st c)))))

(define (max-length arr)
  (let go ([cur (set)] [strs arr])
    (if (null? strs)
        (set-count cur)
        (let ([maybe-cs (charset-add cur (car strs))])
          (if maybe-cs
              (max (go maybe-cs (cdr strs))
                   (go cur (cdr strs)))
              (go cur (cdr strs)))))))
