#lang racket

(require racket/set)
(require "utils.rkt")

(day 10)
(testing #f)

;; Part One                
(define (get-trailheads chars rows cols)
  (flatten (for*/list ([i (range rows)]
                      [j (range cols)])
             (if (equal? #\0 (2d-ref chars i j)) (point i j) '()))))

(define (diagonal? dr dc)
  (and (equal? 1 (abs dr)) (equal? 1 (abs dc))))

(define (traverse chars i j p2?)
  (match (2d-ref chars i j)
    [#\9 ((if p2? list set) (point i j))]
    [cur (for*/fold
              ([terminals (if p2? '() (set))])
              ([i^ (inclusive-range -1 1)]
               [j^ (inclusive-range -1 1)])
           ((if p2? append set-union) terminals
                      (if (and (not (diagonal? i^ j^))
                               (equal? (integer->char (add1 (char->integer cur)))
                                       (2d-ref-default chars (+ i i^) (+ j j^) #\0)))
                          (traverse chars (+ i i^) (+ j j^) p2?)
                          (if p2? '() (set)))))]))

(printf "Part one: ~a\n" (sum (for/list
                                  ([th (get-trailheads chars rows cols)])
                                (set-count (traverse chars (point-x th) (point-y th) #f)))))

;; Part Two
(printf "Part two: ~a\n" (sum (for/list
                                  ([th (get-trailheads chars rows cols)])
                                (length (traverse chars (point-x th) (point-y th) #t)))))
