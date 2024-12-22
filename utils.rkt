#lang racket

(require racket/struct)
(require net/url)
(require racket/format)

(require "private.rkt")

(provide in-file lines chars rows cols day testing
         id char-num? 2d-ref 2d-ref-default sub-vec symbol-append
         number->symbol dict-filter dict-append flip-dict
         uniquify-name sum in-bounds 2d-vec-copy 2d-set!
         set-map->set set-filter set-filter-map set-flatten
         ls-of b->n sub-list
         (struct-out point))

;; General utils
(struct point (x y) #:transparent)

(define (id x) x)
(define (sum ls) (foldr + 0 ls))
(define (char-num? c) (char<=? #\0 c #\9))
(define (in-bounds i j m) (and (0 . <= . i) (i . < . (vector-length m))
                               (0 . <= . j) (j . < . (vector-length (vector-ref m 0)))))
(define (2d-ref m i j [d #f]) (if (in-bounds i j m) (vector-ref (vector-ref m i) j) d))
(define (2d-ref-default m i j default) (if (in-bounds i j m) (2d-ref m i j) default)) ;; deprecated
(define (2d-vec-copy m) (for/vector ([v m]) (vector-copy v)))
(define (2d-set! m i j v) (vector-set! (vector-ref m i) j v))
(define (sub-vec v si ee) (vector-drop (vector-take v ee) si))
(define (symbol-append s1 s2) (string->symbol (string-append (symbol->string s1) (symbol->string s2))))
(define (number->symbol n) (string->symbol (number->string n)))
(define (ls-of l e) (build-list l (λ (x) e)))
(define (sub-list l s e) (drop (take l e) s))

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

(define (set-map->set proc st) (for/set ([x st]) (proc x)))
(define (set-filter proc st) (for/fold ([fs (set)]) ([x st]) (if (proc x) (set-add fs x) fs)))
(define (set-filter-map proc st)
  (for/fold ([fs (set)]) ([x st]) (let ((px (proc x))) (if px (set-add fs px) fs))))
(define (set-flatten st)
  (for/fold ([fl (set)]) ([x st]) (set-union fl (if (set? x) (set-flatten x) (set x)))))

(define unique-number 0)
(define (uniquify-name x)
  (begin
    (set! unique-number (add1 unique-number))
    (symbol-append x (symbol-append (string->symbol ".") (number->symbol unique-number)))))

(define (b->n b) (if b 1 0))

;; Input Api (Thanks calcin)
(define (make-input-request day)
  (port->string (get-pure-port (string->url (format "https://adventofcode.com/2024/day/~a/input" day))
                               (list (format "Cookie: ~a" PRIV_COOKIE)))))

(define (pad-day day)
  (~a day #:min-width 2 #:align 'right #:left-pad-string "0"))

(define (get-input-path day)
  (format "inputs/in_~a.txt" (pad-day day)))

(define (get-input fname day)
  (if (file-exists? fname)
      (file->lines fname)
      (let ([raw (make-input-request day)])
        (begin
          (display-to-file raw fname)
          (string-split raw "\n")))))

;; File utils
(define (update-input)
  (set! lines (get-input input-file day-num))
  (set! chars (list->vector (map (compose list->vector string->list) lines)))
  (set! rows (vector-length chars))
  (set! cols (vector-length (vector-ref chars 0))))

(define day-num 1)
(define input-file "inputs/in_01.txt")

(define lines #f)
(define chars #f)
(define rows  #f)
(define cols  #f)
(update-input)

(define (day n)
  (set! day-num n)
  (update-input))

(define (testing b)
  (set! input-file (if b "inputs/in-test.txt" (get-input-path day-num)))
  (update-input))

(define (in-file p) ;; deprecated
  (set! input-file (string-append "inputs/" p))
  (update-input))
