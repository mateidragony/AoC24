#lang racket

(require racket/set)
(require "utils.rkt")

(day 11)
(testing #f)

;; Part One                
(define stones (map string->number (string-split (first lines) " ")))

(define (blink stone)
  (define stone-str (number->string stone))
  (define stone-len (string-length stone-str))
  (cond
    ((zero? stone) (list 1))
    ((even? stone-len) (map string->number
                            (list (substring stone-str 0 (quotient stone-len 2))
                                  (substring stone-str (quotient stone-len 2)))))
    (else (list (* 2024 stone)))))

(define memo (make-hash))

(define (blink-n n stone)
  (cond
    ((zero? n) 1)
    ((dict-has-key? memo (point stone n)) (dict-ref memo (point stone n)))
    (else (define ret (sum (map (curry blink-n (sub1 n)) (blink stone))))
          (dict-set! memo (point stone n) ret)
          ret)))

(printf "Part one: ~a\n" (sum (map (curry blink-n 25) stones)))

;; Part Two
(printf "Part two: ~a\n" (sum (map (curry blink-n 75) stones)))
