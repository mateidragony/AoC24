#lang racket

(require racket/set)
(require "utils.rkt")

(day 14)
(testing #f)

;; Part One
(define width  101)
(define height 103)

(struct guard (p v) #:transparent)

(define (parse-input lines)
  (match lines
    ['() '()]
    [(cons line lines)
     (match-let ([(list _ px py vx vy) (regexp-match #px"p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)" line)])
       (cons (guard (point (string->number px) (string->number py))
                    (point (string->number vx) (string->number vy)))
             (parse-input lines)))]))

(define (move g)
  (define x  (point-x (guard-p g))) (define y  (point-y (guard-p g)))
  (define dx (point-x (guard-v g))) (define dy (point-y (guard-v g)))
  (guard (point (modulo (+ x dx) width) (modulo (+ y dy) height)) (guard-v g)))

(define (move-n g n)
  (if (zero? n) g (move-n (move g) (sub1 n))))

(define (find-loop g s)
  (if (equal? (guard-p g) s) 1 (add1 (find-loop (move g) s))))

(define (run-seconds guards seconds)
  (for/list ([g guards])
    (move-n g (modulo seconds (find-loop (move g) (guard-p g))))))

(define (count-quadrants guards)
  (match guards
    ['() '(0 0 0 0)]
    [(cons g guards)
     (define rec (count-quadrants guards))
     (define-values (x y) (values (point-x (guard-p g)) (point-y (guard-p g))))
     (define idx (+ (if (< x (quotient width 2))  1 0)
                    (if (< y (quotient height 2)) 2 0)))
     (if (or (equal? x (quotient width 2)) (equal? y (quotient height 2)))
         rec (list-set rec idx (add1 (list-ref rec idx))))]))

(define guards (parse-input lines))

(printf "Part one: ~a\n" (apply * 1 (count-quadrants (run-seconds guards 100))))

;; Part Two

(define (print-state guards i)
  (define grid (list->vector (for/list ([i (range height)]) (make-vector width #\.))))
  (for ([g guards])
    (2d-set! grid (point-y (guard-p g)) (point-x (guard-p g)) #\#))
  (when (is-picture? grid guards)
    (printf "\nAfter ~a seconds:\n" i)
    (for ([line grid])
      (for ([c line]) (printf "~a" c))
      (printf "\n"))))

(define (neighbors grid i j)
  (for*/list ([i^ (inclusive-range -1 1)]
              [j^ (inclusive-range -1 1)])
    (2d-ref-default grid (+ i i^) (+ j j^) #\.)))

(define (num-close-guards grid guards)
  (match guards
    ['() 0]
    [(cons g guards)
     (+ (if (> (length (filter (curry equal? #\#)
                               (neighbors grid (point-y (guard-p g)) (point-x (guard-p g))))) 1)
            1 0) 
        (num-close-guards grid guards))]))

(define (is-picture? grid guards)
  (> (num-close-guards grid guards) 163))

(define state guards)
(for ([i (range 6878)])
  (set! state (map move guards))

  (print-state state i))

(printf "Part two: ~a\n" 0)
