#lang racket

(require racket/set)
(require "utils.rkt")

(day 8)
(testing #f)

;; Part One                
(define (collect-antennas chars [i 0] [j 0])
  (cond
    ((>= i rows) (make-immutable-hash))
    ((>= j cols) (collect-antennas chars (add1 i) 0))
    (else
     (define cur (2d-ref chars i j))
     (define ret (collect-antennas chars i (add1 j)))
     (if (not (equal? cur #\.))
         (dict-set ret cur (set-add (dict-ref ret cur (set)) (point i j)))
         ret))))

(define (get-antinodes p1 p2 chars) 
  (define dr (- (point-x p1) (point-x p2)))
  (define dc (- (point-y p1) (point-y p2)))
  (set-subtract (set (point (+ (point-x p1) dr) (+ (point-y p1) dc))
                     (point (+ (point-x p2) dr) (+ (point-y p2) dc))
                     (point (- (point-x p1) dr) (- (point-y p1) dc))
                     (point (- (point-x p2) dr) (- (point-y p2) dc)))
                (set p1 p2)))

(define (get-antenna-antinodes antennas chars get-antinodes-fn)
  (flatten (for*/list ([a1 antennas]
                       [a2 antennas])
             (set->list (get-antinodes-fn a1 a2 chars)))))

(define (get-all-antinodes chars all-antennas get-antinodes-fn)
  (list->set
   (filter (λ (p) (in-bounds (point-x p) (point-y p) chars))
           (flatten (for/list ([antenna (dict-keys all-antennas)])
                      (get-antenna-antinodes (dict-ref all-antennas antenna) chars get-antinodes-fn))))))

(printf "Part one: ~a\n" (set-count (get-all-antinodes chars (collect-antennas chars) get-antinodes)))

;; Part Two
(define (get-line i j dr dc chars)
  (cond
    ((not (in-bounds i j chars)) (set))
    (else (set-add (get-line (+ i dr) (+ j dc) dr dc chars) (point i j)))))

(define (get-antinodes^ p1 p2 chars)
  (cond
    ((equal? p1 p2) (set))
    (else
     (define dr (- (point-x p1) (point-x p2)))
     (define dc (- (point-y p1) (point-y p2)))
     (set-union (get-line (point-x p1) (point-y p1) dr dc chars)
                (get-line (point-x p1) (point-y p1) (* -1 dr) (* -1 dc) chars)))))

(printf "Part two: ~a\n" (set-count (get-all-antinodes chars (collect-antennas chars) get-antinodes^)))
