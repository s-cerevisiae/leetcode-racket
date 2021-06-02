#lang racket

(define/contract (longest-common-prefix/cheat strs)
  (-> (listof string?) string?)
  (match (map string->list strs)
    ['() ""]
    [(cons fst rst)
     (list->string (foldr take-common-prefix fst rst))]))
