#lang racket

(require racket/set)

;; General utils
(struct point (x y) #:transparent)

(define (id x) x)
(define (comp f g) (λ (x) (f (g x))))
(define (char-num? c) (char<=? #\0 c #\9))
(define (2d-ref m i j) (vector-ref (vector-ref m i) j))
(define (sub-vec v si ee) (vector-drop (vector-take v ee) si))
(define (symbol-append s1 s2) (string->symbol (string-append (symbol->string s1) (symbol->string s2))))
(define (number->symbol n) (string->symbol (number->string n)))

(define (dict-filter p d)
  (cond ((dict-empty? d) d)
        (else (define k (car (dict-keys d)))
              (if (p k (dict-ref d k))
                  (dict-set (dict-filter p (dict-remove d k)) k (dict-ref d k))
                  (dict-filter p (dict-remove d k))))))
(define (dict-combine-key k d1 d2)
  (define v1 (dict-ref d1 k))
  (define v2 (dict-ref d2 k))
  (cond
    ((list? v1) (append v1 v2))
    ((set?  v1) (set-union v1 v2))
    (else v1)))
(define (dict-append-two d1 d2)
  (cond
    ((dict-empty? d1) d2)
    (else (define k (car (dict-keys d1)))
          (dict-append-two (dict-remove d1 k) (if (dict-has-key? d2 k)
                                                  (dict-set d2 k (dict-combine-key k d1 d2))
                                                  (dict-set d2 k (dict-ref d1 k)))))))
(define (dict-append . ds)
  (match ds
    ['() '()]
    [(cons d '()) d]
    [(cons d ds) (dict-append-two d (apply dict-append ds))]))
(define (flip-dict d empty-dict)
   (foldr (λ (x d) (dict-set d (cdr x) (car x))) empty-dict (dict->list d)))

(define unique-number 0)
(define (uniquify-name x)
  (begin
    (set! unique-number (add1 unique-number))
    (symbol-append x (symbol-append (string->symbol ".") (number->symbol unique-number)))))

;; File utils
(define in-file "intest.txt")
(define lines (sequence->list (in-lines (open-input-file in-file))))

(define chars (list->vector (map (comp list->vector string->list) lines)))
(define rows (vector-length chars))
(define cols (vector-length (vector-ref chars 0)))

;; Part One                
(printf "Part one: ~a\n" 0)

;; Part Two
(printf "Part two: ~a\n" 0)
