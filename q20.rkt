#lang racket

(require racket/set)
(require "utils.rkt")

(day 20)
(testing #f)

;; Part One
(struct cell (p s) #:transparent)

(define (get-start/end grid [i 0] [j 0] [s #f] [e #f])
  (cond
    ((equal? i rows) (values s e))
    ((equal? j cols) (get-start/end grid (add1 i) 0 s e))
    ((equal? #\S (2d-ref grid i j)) (get-start/end grid (add1 i) 0 (point i j) e))
    ((equal? #\E (2d-ref grid i j)) (get-start/end grid (add1 i) 0 s (point i j)))
    (else (get-start/end grid i (add1 j) s e))))

(define (get-neighbors i j [dist 1])
  (list (point (+ i dist) j) (point (- i dist) j) (point i (+ j dist)) (point i (- j dist))))

(define visited (make-hash))
(define (search! grid q)
  (match q
    ['() (void)]
    [(cons (cell p s) q) #:when (dict-has-key? visited p) (search! grid q)]
    [(cons (cell (point i j) s) q)
     (define ns (filter (λ (p) (not (equal? #\# (2d-ref grid (point-x p) (point-y p) #\#))))
                        (get-neighbors i j)))
     (dict-set! visited (point i j) s)
     (search! grid (map (λ (n) (cell n (+ 1 s))) ns))]))

(define-values (start end) (get-start/end chars))
(search! chars (list (cell start 0)))

(define (dist p1 p2)
  (+ (abs (- (point-x p1) (point-x p2))) (abs (- (point-y p1) (point-y p2)))))

(define (all-pairs-saved ps n)
  (match ps
    ['() 0]
    [(cons p ps)
     (+ (for/sum ([p^ ps])
          (b->n (and (<= (dist p p^) n)
                     (>= (- (abs (- (dict-ref visited p) (dict-ref visited p^)))
                            (dist p p^))
                         100))))
        (all-pairs-saved ps n))]))

(printf "Part one: ~a\n" (all-pairs-saved (dict-keys visited) 2))

;; Part Two
(printf "Part two: ~a\n" (all-pairs-saved (dict-keys visited) 20))


