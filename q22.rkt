#lang racket

(require racket/set)
(require "utils.rkt")

(day 22)
(testing #f)

;; Part One
(define num-secs 2000)

(define (mix v s) (bitwise-xor v s))
(define (prune s) (modulo s 16777216))

(define (secret s)
  (define s^ (prune (mix (* s 64) s)))
  (define s^^ (prune (mix (quotient s^ 32) s^)))
  (prune (mix (* s^^ 2048) s^^)))

(define (secret-n s n)
  (cond
    ((zero? n) s)
    (else (secret-n (secret s) (sub1 n)))))

(printf "Part one: ~a\n" (for/sum ([line lines]) (secret-n (string->number line) num-secs)))

;; Part Two
(define (get-bananas s n)
  (cond
    ((zero? n) (list (modulo s 10)))
    (else
     (cons (modulo s 10) (get-bananas (secret s) (sub1 n))))))

(define (diffs/naners s n)
  (define bananas (get-bananas s n))
  (values (for/list ([sec (drop bananas 1)] [prev (drop-right bananas 1)])
            (- sec prev))
          bananas))

(define-values (diffs bananas)
  (for/lists (dss bss)
             ([line lines])
    (diffs/naners (string->number line) num-secs)))

(define (count-seqs ds bs seqs seqs->nana)
  (match* (ds bs)
    [(_ '()) #:when (< (length ds) 4) (void)]
    [((cons d ds) (cons b bs))
     #:when (set-member? seqs (take (cons d ds) 4))
     (count-seqs ds bs seqs seqs->nana)]
    [((cons d ds) (cons b bs))
     (define seq (take (cons d ds) 4))
     (set-add! seqs seq)
     (dict-set! seqs->nana seq (+ b (dict-ref seqs->nana seq 0)))
     (count-seqs ds bs seqs seqs->nana)]))

(define seqs (mutable-set))
(define seqs->nana (make-hash))
(for ([ds diffs]
      [bs bananas])
  (set-clear! seqs)
  (count-seqs ds (drop bs 4) seqs seqs->nana))

(printf "Part two: ~a\n" (foldr max 0 (dict-values seqs->nana)))
