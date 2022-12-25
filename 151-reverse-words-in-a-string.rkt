#lang racket

(define (reverse-words s)
  (string-join
    (reverse
      (string-split s))))
