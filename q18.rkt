#lang racket

(require racket/set)
(require "utils.rkt")

(day 18)
(testing #f)

;; Part One
(struct cell (p s path) #:transparent)

(define width 71)
(define height 71)

(define grid (for/vector ([i (range width)]) (make-vector height #\.)))

(define (simulate-n grid coords n)
  (cond
    ((zero? n) grid)
    (else
     (match-define (list x y) (string-split (car coords) ","))
     (define grid^ (2d-vec-copy grid))
     (2d-set! grid^ (string->number y) (string->number x) #\#)
     (simulate-n grid^ (cdr coords) (sub1 n)))))

(define (get-neighbors i j)
  (list (point (add1 i) j) (point (sub1 i) j) (point i (add1 j)) (point i (sub1 j))))

(define (search grid q end visited)
  (match q
    ['() #f]
    [(cons (cell p s path) q) #:when (equal? p end) path]
    [(cons (cell p s path) q) #:when (set-member? visited p) (search grid q end visited)]
    [(cons (cell (point x y) s path) q)
     (define ns (filter (λ (p) (not (equal? #\# (2d-ref-default grid (point-y p) (point-x p) #\#))))
                        (get-neighbors x y)))
     (search grid (append q (map (λ (pt) (cell pt (add1 s) (cons (point x y) path))) ns))
             end (set-add visited (point x y)))]))

(define simulated (simulate-n grid lines 1024))

(define path (search simulated (list (cell (point 0 0) 0 (list))) (point (sub1 width) (sub1 height)) (set)))

(printf "Part one: ~a\n" (length path))

;; Part Two
(define (search-n grid lines)
  (define grid^ (simulate-n grid lines 1))
  (if (search grid^
              (list (cell (point 0 0) 0 (list)))
              (point (sub1 width) (sub1 height)) (set))
      (search-n grid^ (cdr lines))
      (car lines)))

(printf "Part two: ~a\n" (search-n grid lines))
