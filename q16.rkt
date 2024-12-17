#lang racket

(require racket/set)
(require data/heap)
(require graph)
(require "utils.rkt")

(day 16)
(testing #f)

;; Part One
(struct cell (p d) #:transparent)

(define (count-hash grid [i 0] [j 0])
  (cond
    ((equal? i rows) 2)
    ((equal? j cols) (count-hash grid (add1 i) 0))
    ((equal? #\. (2d-ref grid i j)) (add1 (count-hash grid i (add1 j))))
    (else (count-hash grid i (add1 j)))))

(define (get-start/end grid [i 0] [j 0] [s #f] [e #f])
  (cond
    ((equal? i rows) (values s e))
    ((equal? j cols) (get-start/end grid (add1 i) 0 s e))
    ((equal? #\S (2d-ref grid i j)) (get-start/end grid (add1 i) 0 (point i j) e))
    ((equal? #\E (2d-ref grid i j)) (get-start/end grid (add1 i) 0 s (point i j)))
    (else (get-start/end grid i (add1 j) s e))))

(define (get-neighbors i j)
  (list (cell (point (add1 i) j) (point 1 0)) (cell (point (sub1 i) j) (point -1 0))
        (cell (point i (add1 j)) (point 0 1)) (cell (point i (sub1 j)) (point 0 -1))))


(define (add-edges! grid g q)
  (cond
    ((set-empty? q) g)
    (else
     (match-define (cell p d) (set-first q))
     (define ns (filter (λ (c) (and
                                (not (equal? #\# (2d-ref grid
                                                         (point-x (cell-p c))
                                                         (point-y (cell-p c)))))
                                (not (equal? (point (- (point-x d)) (- (point-y d)))
                                             (cell-d c)))))
                        (get-neighbors (point-x p) (point-y p))))
     (define ns-to-go (mutable-set))
     (for ([n ns])
       (define weight (if (equal? d (cell-d n)) 1 1001))
       (when (not (has-edge? g (cell p d) n))
         (add-edge! g (cell p d) n weight)
         (set-add! ns-to-go n)))
     (add-edges! grid g (set-union (set-remove q (cell p d))
                                   ns-to-go)))))

(define (build-graph grid s)
  (define g (weighted-graph/undirected '()))
  (add-edges! grid g (set (cell s (point 0 1)))))

(define-values (start end) (get-start/end chars))
(define graph (build-graph chars start))
(define-values (dist prev) (dijkstra graph (cell start (point 0 1))))
(define cost (inexact->exact
              (min (dict-ref dist (cell end (point  0  1)) +inf.0)
                   (dict-ref dist (cell end (point  1  0)) +inf.0)
                   (dict-ref dist (cell end (point  0 -1)) +inf.0)
                   (dict-ref dist (cell end (point -1  0)) +inf.0))))

(printf "Part one: ~a\n" cost)

;; Part Two
(define directions (list (point 1 0) (point -1 0) (point 0 1) (point 0 -1)))

(define (get-seats q s dist prev)
  (cond
    ((set-empty? q) (set))
    (else
     (match-define (list cur pc pd) (set-first q))
     (define paths (for/fold ([next (set)]) ([d directions])
                     (cond
                       ((equal? cur s) next)
                       ((equal? (if pd (- pc (if (equal? pd d) 1 1001)) pc)
                                (dict-ref dist (cell cur d) +inf.0))
                        (set-add next (list (cell-p (dict-ref prev (cell cur d)))
                                            (dict-ref dist (cell cur d)) d)))
                       (else next))))
     (set-add (get-seats (set-union (set-remove q (list cur pc pd)) paths) s dist prev) cur))))

(define seats (get-seats (set (list end cost #f)) start dist prev))

(printf "Part two: ~a\n" (set-count seats))
