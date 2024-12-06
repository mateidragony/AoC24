#lang racket

(require racket/set)
(require "utils.rkt")

(day 6)
(testing #f)

;; Part One
(define (find-start chars [i 0] [j 0])
  (cond
    ((equal? j cols) (find-start chars (add1 i) 0))
    ((equal? #\^ (2d-ref chars i j)) (values i j -1 0))
    ((equal? #\v (2d-ref chars i j)) (values i j  1 0))
    ((equal? #\< (2d-ref chars i j)) (values i j  0 -1))
    ((equal? #\> (2d-ref chars i j)) (values i j  0 1))
    (else (find-start chars i (add1 j)))))

(define (next-ds dr dc)
  (match* (dr dc)
    [(-1  0) (values  0  1)]
    [( 0  1) (values  1  0)]
    [( 1  0) (values  0 -1)]
    [( 0 -1) (values -1  0)]))

(define (go chars i j dr dc)
  (define-values (i^ j^) (values (+ i dr) (+ j dc)))
  (define-values (dr^ dc^) (next-ds dr dc))
  (cond
    ((not (in-bounds i^ j^ chars)) (set (point i j)))
    ((equal? #\# (2d-ref chars i^ j^)) (go chars i j dr^ dc^))
    (else (set-add (go chars i^ j^ dr dc) (point i j)))))

(define-values (si sj dr dc) (find-start chars))

(define normal-path (go chars si sj dr dc))
(printf "Part one: ~a\n" (set-count normal-path))

;; Part Two
(define (go^ chars i j dr dc memo)
  (define-values (i^ j^) (values (+ i dr) (+ j dc)))
  (define-values (dr^ dc^) (next-ds dr dc))
  (cond
    ((not (in-bounds i^ j^ chars)) #f)
    ((equal? #\# (2d-ref chars i^ j^)) (go^ chars i j dr^ dc^ memo))
    (else
     (define memo^ (set-add memo (cons (point i j) (point dr dc))))
     (if (equal? (set-count memo) (set-count memo^)) #t (go^ chars i^ j^ dr dc memo^)))))

(define p2 (length (filter id (for/list ([p (set-remove normal-path (point si sj))])
                                (define chars^ (2d-vec-copy chars))
                                (2d-set! chars^ (point-x p) (point-y p) #\#)
                                (go^ chars^ si sj dr dc (set))))))

;; slow asl but works :)
(printf "Part two: ~a\n" p2)
