#lang racket

(require racket/set)
(require "utils.rkt")

(day 2)
(testing #f)

;; Part One
(define (is-safe line p1?)
  (letrec ((rec (λ (line prev inc? removed?)
                  (match line
                    ['() #t]
                    [(cons x line)
                     (define test (and (<= 1 (abs (- prev x)) 3)
                                       (if inc? (< prev x) (> prev x))))
                     (cond
                       (test (rec line x inc? removed?))
                       ((not removed?) (rec line prev inc? #t))
                       (removed? #f))]))))
    (if p1?
        (rec (cdr line) (first line) (< (first line) (second line)) #t)
        (or (rec (cdr line) (first line) (< (first line) (second line)) #f)
            (rec (cddr line) (first line) (< (first line) (third line)) #t)
            (is-safe (cdr line) #t)))))

(printf "Part one: ~a\n" (length (filter (λ (line)
                                           (is-safe (map string->number (string-split line)) #t))
                                         lines)))

;; Part Two
(printf "Part two: ~a\n" (length (filter (λ (line)
                                           (is-safe (map string->number (string-split line)) #f))
                                         lines)))

