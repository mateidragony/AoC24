#lang racket

(require racket/set)
(require "utils.rkt")

(day 5)
(testing #f)

;; Part One
(define-values (ruleStrs pageStrs) (values (takef lines (λ (x) (not (equal? x ""))))
                                     (cdr (dropf lines (λ (x) (not (equal? x "")))))))

(define (get-rules trans?)
  (for/fold ([d (make-immutable-hash)])
            ([rule ruleStrs])
    (define splt (if trans? (reverse (string-split rule "|"))
                            (string-split rule "|")))
    (dict-append
     d (dict-set (make-immutable-hash)
                 (string->number (first splt))
                 (set (string->number (second splt)))))))

(define rules (get-rules #f))

(define rules^ (get-rules #t))

(define pages (for/list ([page pageStrs])
                (map string->number (string-split page ","))))

(define (invalid-number n rules befores)
  (not (set-empty? (set-intersect befores (dict-ref rules n (set))))))

(define (page-valid? rules befores)
  (λ (page)
    (match page
      ['() #t]
      [(cons n page) #:when (invalid-number n rules befores) #f]
      [(cons n page) ((page-valid? rules (set-add befores n)) page)])))

(define (get-middle ls)
  (list-ref ls (quotient (length ls) 2)))

(printf "Part one: ~a\n" (sum (map get-middle (filter (page-valid? rules (set)) pages))))

;; Part Two
(define (fix-page rules acc)
  (λ (page)
    (match page
      ['() acc]
      [(cons n page) #:when (invalid-number n rules (list->set page))
                     ((fix-page rules acc)
                      (append page (list n)))]
      [(cons n page) ((fix-page rules (cons n acc)) page)])))

(printf "Part two: ~a\n" (sum (map (compose get-middle (fix-page rules^ '()))
                                   (filter (λ (p) (not ((page-valid? rules (set)) p))) pages))))
