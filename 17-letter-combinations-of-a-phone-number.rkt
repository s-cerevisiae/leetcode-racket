#lang racket

(define letter-map
  #hash((#\2 . (#\a #\b #\c))
        (#\3 . (#\d #\e #\f))
        (#\4 . (#\g #\h #\i))
        (#\5 . (#\j #\k #\l))
        (#\6 . (#\m #\n #\o))
        (#\7 . (#\p #\q #\r #\s))
        (#\8 . (#\t #\u #\v))
        (#\9 . (#\w #\x #\y #\z))))

(define/contract (letter-combinations digits)
  (-> string? (listof string?))
  (if (zero? (string-length digits))
      '()
      (map list->string
        (combine (string->list digits)))))

(define (combine ns)
  (define (get-letters n) (hash-ref letter-map n))
  (if (null? ns)
      '(())
      (for*/list ([rest-combinations (combine (cdr ns))]
                  [current-letter (get-letters (car ns))])
        (cons current-letter rest-combinations))))
