#lang racket

(require racket/set)
(require "utils.rkt")

(day 13)
(testing #f)

;; Part One                
(define (parse-input lines p1?)
  (define-values (info rst) (splitf-at lines (curry (compose not equal?) "")))
  (cond
    ((null? rst) (list (parse-machine info p1?)))
    (else 
     (cons (parse-machine info p1?) (parse-input (cdr rst) p1?)))))

(define (parse-machine info p1?)
  (match-let ([(list _ ax ay) (regexp-match #px"Button A: X\\+(\\d+), Y\\+(\\d+)" (first info))]
              [(list _ bx by) (regexp-match #px"Button B: X\\+(\\d+), Y\\+(\\d+)" (second info))]
              [(list _ px py) (regexp-match #px"Prize: X=(\\d+), Y=(\\d+)"        (third info))])
    (dict-set* (make-immutable-hash)
               'a (point (string->number ax) (string->number ay))
               'b (point (string->number bx) (string->number by))
               'p (point (+ (if p1? 0 10000000000000) (string->number px))
                         (+ (if p1? 0 10000000000000) (string->number py))))))

(define (tokens-machine a b p)
  (define m1 (/ (point-y a) (point-x a)))
  (define m2 (/ (point-y b) (point-x b)))
  (define b2 (- (point-y p) (* m2 (point-x p))))
  (define x-int (/ b2 (- m1 m2)))
  (cond
    ((not (integer? x-int)) 0)
    (else (define na (quotient x-int (point-x a)))
          (define nb (quotient (- (point-x p) x-int) (point-x b)))
          (if (equal? (point-y p) (+ (* na (point-y a)) (* nb (point-y b))))
              (+ (* 3 na) nb)
              0))))

(define num-tokens
  (for/list ([machine (parse-input lines #t)])
    (tokens-machine (dict-ref machine 'a) (dict-ref machine 'b) (dict-ref machine 'p))))

(printf "Part one: ~a\n" (sum num-tokens))

;; Part Two
(define num-tokens^
  (for/list ([machine (parse-input lines #f)])
    (tokens-machine (dict-ref machine 'a) (dict-ref machine 'b) (dict-ref machine 'p))))

(printf "Part two: ~a\n" (sum num-tokens^))
