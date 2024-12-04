#lang racket

(require racket/set)
(require "utils.rkt")

(day 3)
(testing #f)

;; Part One                
(define mul-re #rx"mul\\([0-9]+,[0-9]+\\)")

(define (sum-line line)
  (sum (for/list ([instr (regexp-match* mul-re line)])
         (foldr * 1 (map string->number
                         (regexp-match* #rx"[0-9]+" instr))))))

(define input (foldr string-append "" lines))

(printf "Part one: ~a\n" (sum-line input))

;; Part Two

(define (dos in do?)
  (cond
    ((null? in) '())
    (else
     (define splt (string-split in (if do? "don't()" "do()")))
     (cond
       ((null? splt) '())
       (else
        (define ret (dos (foldr (λ (a b)
                                  (string-append a (if do? "don't()" "do()") b))
                                "" (cdr splt)) (not do?)))
        (if do? (cons (car splt) ret) ret))))))

(printf "Part two: ~a\n" (sum (map sum-line (dos input #t))))
