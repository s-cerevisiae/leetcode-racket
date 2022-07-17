#lang racket

(define (calculate s)
  (define-values (result _)
    (parse (filter-not char-whitespace? (string->list s))))
  result)

(define (parse l)
  (define (parse-num l)
    (define (char->digit c)
      (- (char->integer c) 48))
    (let loop ([l l] [result 0])
      (match l
        [(cons (and c (? char-numeric?)) cs)
         (loop cs (+ (char->digit c)
                     (* 10 result)))]
        [_ (values result l)])))

  (define (parse1 l)
    (match l
      [(cons (? char-numeric?) _)
       (parse-num l)]
      [(cons #\( cs)
       (define-values (val rest) (parse cs))
       (values val (cdr rest))]
      [(cons #\- cs)
       (define-values (val rest) (parse1 cs))
       (values (- val) rest)]))

  (define-values (lhs rest) (parse1 l))
  (let loop ([lhs lhs] [l rest])
    (match l
      [(or '() (cons #\) _))  (values lhs l)]
      [(cons #\+ cs)
       (define-values (rhs rest) (parse1 cs))
       (loop (+ lhs rhs) rest)]
      [(cons #\- cs)
       (define-values (rhs rest) (parse1 cs))
       (loop (- lhs rhs) rest)])))
