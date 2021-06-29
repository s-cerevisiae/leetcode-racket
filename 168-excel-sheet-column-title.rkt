#lang racket

(define (convert-to-title column-num)
  (list->string
    (let loop ([n column-num] [title '()])
      (if (zero? n)
          title
          (let-values ([(q r) (quotient/remainder (- n 1) 26)])
            (loop q (cons (integer->char (+ r 65))
                          title)))))))
