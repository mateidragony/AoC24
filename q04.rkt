#lang racket

(require racket/set)
(require "utils.rkt")

(day 4)
(testing #f)

;; Part One
(define xmas (list #\X #\M #\A #\S))

(define (check-word-dir chars i j dr dc letters)
  (cond ((null? letters) 1)
        ((not (in-bounds i j chars)) 0)
        ((not (equal? (car letters) (2d-ref chars i j))) 0)
        (else (check-word-dir chars (+ i dr) (+ j dc) dr dc (cdr letters)))))

(define (check-word chars i j)
  (cond ((not (equal? #\X (2d-ref chars i j))) 0)
        (else
         (sum (for*/list ([dr (inclusive-range -1 1)]
                          [dc (inclusive-range -1 1)])
                (check-word-dir chars (+ i dr) (+ j dc) dr dc (cdr xmas)))))))

(define (check-each check-fn)
  (for*/list ([i (range 0 rows)]
              [j (range 0 cols)])
    (check-fn chars i j)))

(printf "Part one: ~a\n" (sum (check-each check-word)))

;; Part Two
(define (check-word2 chars i j)
  (cond ((not (equal? #\A (2d-ref chars i j))) 0)
        (else
         (define d1 (list (2d-ref-default chars (sub1 i) (sub1 j) #\X)
                          (2d-ref-default chars (add1 i) (add1 j) #\X)))
         (define d2 (list (2d-ref-default chars (sub1 i) (add1 j) #\X)
                          (2d-ref-default chars (add1 i) (sub1 j) #\X)))
         (define correct (list #\M #\S))
         (if (and (equal? (sort d1 char<?) correct) (equal? (sort d2 char<?) correct))
             1 0))))

(printf "Part two: ~a\n" (sum (check-each check-word2)))
