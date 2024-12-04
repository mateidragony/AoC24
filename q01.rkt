#lang racket

(require racket/set)
(require "utils.rkt")

(day 1)
(testing #f)

;; Part One                
(define num-cols (for/list ([l lines]) (string-split l)))
(define trans-cols (apply map list num-cols))
(define cols-nums (map (λ (x) (map string->number x)) trans-cols))
(define sorted-cols (map (λ (l) (sort l <)) cols-nums))
(define diff (for/list ([c1 (first sorted-cols)] [c2 (second sorted-cols)])
               (abs (- c1 c2))))
(define sum1 (sum diff))
(printf "Part one: ~a\n" sum1)

;; Part Two
(define (num-occurs x ls)
  (match ls
    ['() 0]
    [(cons a ls) #:when (equal? x a) (add1 (num-occurs x ls))]
    [(cons _ ls) (num-occurs x ls)]))

(define each-occurs (map (λ (x) (num-occurs x (second sorted-cols))) (first sorted-cols)))
(define occur-sum (for/list ([c1 (first sorted-cols)] [o each-occurs])
                    (* c1 o)))
(define sum2 (sum occur-sum))
(printf "Part two: ~a\n" sum2)
