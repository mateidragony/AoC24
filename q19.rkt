#lang racket

(require racket/set)
(require "utils.rkt")

(day 19)
(testing #f)

;; Part One
(define memo (make-hash))
(define (try-patterns pats towel)
  (cond
    ((equal? towel "") 1)
    ((dict-has-key? memo towel) (dict-ref memo towel))
    (else
     (define ret
       (for/sum ([pat pats]
                 #:do
                 [(define pos
                    (and (>= (string-length towel) (string-length pat))
                         (equal? pat (substring towel 0 (string-length pat)))
                         (try-patterns pats (substring towel (string-length pat)))))]
                 #:when pos)
         pos))
     (dict-set! memo towel ret)
     ret)))

(define pats (string-split (first lines) ", "))
(define towels (cddr lines))

(printf "Part one: ~a\n" (length (filter-map (compose (negate zero?) (curry try-patterns pats)) towels)))

;; Part Two
(printf "Part two: ~a\n" (sum (map (curry try-patterns pats) towels)))
