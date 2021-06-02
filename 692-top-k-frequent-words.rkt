#lang racket

(define (top-k-frequent words k)
  (define (count-word words)
    (define table (make-hash))
    (for ([word (in-list words)])
      (hash-update! table word add1 0))
    (hash->list table))
  (define (sort-count counts)
    (sort counts
          (match-lambda**
            [((cons w1 c1) (cons w2 c2))
             (or (> c1 c2)
                 (and (= c1 c2) (string<? w1 w2)))])))
  (map car (take (sort-count (count-word words)) k)))
