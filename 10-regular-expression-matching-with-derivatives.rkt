#lang racket

(define (is-match s p)
  (matches?
    (parse (string->list p))
    (string->list s)))

;; (Listof Char) -> Language
(define (parse pat)
  (define (char c)
    (if (eq? c #\.) 'any c))
  (match pat
    [(list* c #\* rst)
     (make-cat (rep (char c)) (parse rst))]
    [(cons c rst)
     (make-cat (char c) (parse rst))]
    ['() 'ε]))

;; --- Language ---

;; Empty: {No regex}
; #f

;; Epsilon: empty string
; 'ε

;; Any: any single char
; 'any

;; Concatenate: a then b
(struct cat [a b] #:transparent)

;; Alternate: a or b
(define alt set)
(define alt? set?)
(define (alt-map f a)
  (for/foldr ([a (set)])
             ([l (in-set a)])
    (make-alt (f l) a)))
(define (alt-ormap f a)
  (for/or ([l (in-set a)])
    (f l)))
(define alt-union set-union)
(define alt-add set-add)

;; Repeat: lang repeated 0 or more times
(struct rep [lang] #:transparent)

;; ----------------

;; Language Language -> Concatenate
;; Constructs a new `cat` from `a` `b`; eliminates unnecessary nodes
(define (make-cat a b)
  (cond [(eq? 'ε a) b]
        [(eq? 'ε b) a]
        [(not (and a b)) #f]
        [else (cat a b)]))

;; Language Language -> Alternate
;; Constructs a new `alt` from `a` `b`; eliminates unnecessary nodes
(define (make-alt a b)
  (cond [(not (and a b)) (or a b)]
        [(and (alt? a) (alt? b)) (alt-union a b)]
        [(alt? a) (alt-add a b)]
        [(alt? b) (alt-add b a)]
        [else (alt a b)]))

;; Language (Listof Char) -> Bool
(define (matches? lang input)
  (nullable? (foldl derive lang input)))

;; Char Language -> Language
;; Calculates the remaining language after matching `lang` against `char`
(define (derive char lang)
  (let loop ([l lang])
    (match l
      [#f #f]
      ['ε #f]
      [(? alt?) (alt-map loop l)]
      [(cat a b) (if (nullable? a)
                     (make-alt (make-cat (loop a) b)
                               (loop b))
                     (make-cat (loop a) b))]
      [(rep i) (make-cat (loop i) l)]
      ['any 'ε]
      [c (and (eq? c char) 'ε)])))

;; Language -> Bool
;; Tests if `lang` can be null
(define (nullable? lang)
  (match lang
    ['ε #t]
    [(rep _) #t]
    [(? alt?) (alt-ormap nullable? lang)]
    [(cat a b) (and (nullable? a)
                    (nullable? b))]
    [_ #f]))
