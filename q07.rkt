#lang racket

(require racket/set)
(require "utils.rkt")

(day 7)
(testing #f)

;; Part One
(define (calibrate? nums res ops [acc 0])
  (if (> acc res) #f
      (match nums 
        ['() (equal? res acc)]
        [(cons num nums)
         (for/fold ([calibrated #f])
                   ([op ops])
           (or calibrated (calibrate? nums res ops (op acc num))))])))

(define (calibrate-lines lines ops)
  (for/list ([line lines])
    (define-values (res nums) (parse-line line))
    (if (calibrate? nums res ops) res #f)))

(define (parse-line line)
  (define colon (string-split line ":"))
  (values (string->number (first colon))
          (map string->number (string-split (second colon)))))

(printf "Part one: ~a\n" (sum (filter-map id (calibrate-lines lines (list * +)))))

;; Part Two
(define (concat-nums n1 n2)
  (string->number (string-append (number->string n1) (number->string n2))))

(define calibrated^ (for/list ([line lines])
                     (define-values (res nums) (parse-line line))
                     (if (calibrate? nums res (list + * concat-nums)) res #f)))

(printf "Part two: ~a\n" (sum (filter-map id (calibrate-lines lines (list concat-nums * +)))))
