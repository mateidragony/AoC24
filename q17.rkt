#lang racket

(require racket/set)
(require "utils.rkt")

(day 17)
(testing #f)

;;(for ([line lines]) (printf "~a\n" line))

;; Part One
(define (add2 x)
  (+ 2 x))

(define (combo a b c op)
  (match op [4 a] [5 b] [6 c] [else op]))

(define (adv a b c op ip)
  (values (quotient a (expt 2 (combo a b c op))) b c (add2 ip) '()))
(define (bxl a b c op ip)
  (values a (bitwise-xor b op) c (add2 ip) '()))
(define (bst a b c op ip)
  (values a (modulo (combo a b c op) 8) c (add2 ip) '()))
(define (jnz a b c op ip)
  (values a b c (if (zero? a) (add2 ip) op) '()))
(define (bxc a b c op ip)
  (values a (bitwise-xor b c) c (add2 ip) '()))
(define (out a b c op ip)
  (values a b c (add2 ip) (list (modulo (combo a b c op) 8))))
(define (bdv a b c op ip)
  (values a (quotient a (expt 2 (combo a b c op))) c (add2 ip) '()))
(define (cdv a b c op ip)
  (values a b (quotient a (expt 2 (combo a b c op))) (add2 ip) '()))

(define instrs (list adv bxl bst jnz bxc out bdv cdv))

(define (parse-input lines)
  (match-let ([(list _ a) (regexp-match #px"Register A: (\\d+)" (first lines))]
              [(list _ b) (regexp-match #px"Register B: (\\d+)" (second lines))]
              [(list _ c) (regexp-match #px"Register C: (\\d+)" (third lines))]
              [(list _ p) (regexp-match #px"Program: ([\\d+,?]+)" (fifth lines))])
    (values (string->number a) (string->number b) (string->number c)
            (map string->number (string-split p ",")))))

(define (step-program a b c p [ip 0] [out '()])
  (cond
    ((>= ip (sub1 (length p))) (values #f #f #f #f out))
    (else
     (define-values (a^ b^ c^ ip^ out^)
       ((list-ref instrs (list-ref p ip)) a b c (list-ref p (add1 ip)) ip))
     (values a^ b^ c^ ip^ (append out out^)))))

(define (run-program a b c p [ip 0] [out '()])
  (define-values (a^ b^ c^ ip^ out^) (step-program a b c p ip out))
  (if a^ (run-program a^ b^ c^ p ip^ out^) out^))

(define-values (a b c p) (parse-input lines))

(printf "Part one: ~a\n" (string-join (map number->string (run-program a b c p)) ","))

;; Part Two
(define (calc n n^ n^^)
  (bitwise-xor (bitwise-ior
                (arithmetic-shift (bitwise-and n^^ 3) 1)
                (bitwise-and n^ 1))
               (bitwise-xor n 5)))

(define (get-nth-bit x n)
  (arithmetic-shift (bitwise-and x (expt 2 n)) (- n)))

(define (valid-next x a)
  (define c-div (+ (arithmetic-shift                (get-nth-bit a 0) 0)
                   (arithmetic-shift (bitwise-xor 1 (get-nth-bit a 1)) 1)
                   (arithmetic-shift                (get-nth-bit a 2) 2)))
  (equal? x
          (+ (arithmetic-shift (bitwise-xor (bitwise-xor 1 (get-nth-bit a 0))
                                            (get-nth-bit a (+ c-div 0))) 0)
             (arithmetic-shift (bitwise-xor (get-nth-bit a 1)
                                            (get-nth-bit a (+ c-div 1))) 1)
             (arithmetic-shift (bitwise-xor (bitwise-xor 1 (get-nth-bit a 2))
                                            (get-nth-bit a (+ c-div 2))) 2))))

(define (fuck-idk n [as '(0)])
  (cond
    ((zero? n) as)
    (else
     (define nexts (for*/list ([a as]
                               [next (filter (λ (x) (valid-next (bitwise-and n 7)
                                                                (+ (arithmetic-shift a 3) x)))
                                             (range 8))])
                     (+ (arithmetic-shift a 3) next)))
     (fuck-idk (arithmetic-shift n -3) nexts))))

(define (bullshit-ass-study-your-input-no-general-solution-answer p)
  (define num (string->number (string-join (map number->string p) "") 8))
  (fuck-idk num))

(printf "Part two: ~a\n" (car (sort (bullshit-ass-study-your-input-no-general-solution-answer p) <)))
