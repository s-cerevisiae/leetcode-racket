#lang racket

(require "list-node.rkt")

(define (sort-list node)
  (merge-runs (runs node)))

;; List (ListNode) -> ListNode
(define (merge-runs rs)
  (match rs
    ['() #f]
    [(list xs) xs]
    [_ (merge-runs (merge/2 rs))]))

(define (merge/2 rs)
  (match rs
    [(or (list _) '()) rs]
    [(list* xs ys rs)
     (cons (merge xs ys) (merge/2 rs))]))

;; ListNode ListNode -> ListNode
(define/match (merge xs ys)
  [(xs #f) xs]
  [(#f ys) ys]
  [((list-node x xr) (list-node y yr))
   (if (< x y)
       (list-node x (merge xr ys))
       (list-node y (merge xs yr)))])

;; ListNode -> List (ListNode)
(define (runs l)
  (match l
    [(list-node x (list-node y rst))
     (if (< x y)
         (ascend-run x y rst)
         (descend-run x y rst))]
    [(list-node x #f)
     (list l)]
    [#f '()]))

;; Number Number ListNode -> List (ListNode)
(define (ascend-run x y l)
  (let loop ([x y] [l l] [acc (dsingleton x)])
    (match l
      [(list-node y r)
       (if (<= x y)
           (loop y r (dconsr acc x))
           (cons (acc (make-list-node x))
                 (runs l)))]
      [#f (list (acc (make-list-node x)))])))

;; Number Number ListNode -> List (ListNode)
(define (descend-run x y l)
  (let loop ([x y] [l l] [acc (make-list-node x)])
    (match l
     [(list-node y r)
      (if (>= x y)
          (loop y r (list-node x acc))
          (cons (list-node x acc)
                (runs l)))]
     [#f (list (list-node x acc))])))

;; DList = ListNode -> ListNode

;; Number -> DList
(define (dsingleton x)
  (lambda (xs) (list-node x xs)))

;; DList Number -> DList
(define (dconsr xs x)
  (compose1 xs (dsingleton x)))
