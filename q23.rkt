#lang racket

(require racket/set)
(require graph)
(require "utils.rkt")

(day 23)
(testing #f)

;; Part One
(define g (undirected-graph (for/list ([line lines]) (string-split line "-"))))

(define (get-trios g)
  (for*/fold ([trios (set)])
             ([v  (in-vertices g)]
              [n  (in-neighbors g v)]
              [n^ (in-neighbors g n)])
    (if (and (not (equal? v n^)) (member n^ (get-neighbors g v)))
        (set-add trios (set v n n^))
        trios)))

(define (has-chief? s)
  (for/or ([h s]) (equal? #\t (first (string->list h)))))

(printf "Part one: ~a\n" (set-count (set-filter has-chief? (get-trios g))))

;; Part Two
(define (valid? g int)
  (for/and ([v int])
    (equal? int (set-intersect int (list->set (cons v (get-neighbors g v)))))))

(define (biggest g)
  (for*/fold ([p (set)])
             ([v (in-vertices g)]
              [n (in-neighbors g v)])
    (define int (set-intersect (list->set (cons v (get-neighbors g v)))
                               (list->set (cons n (get-neighbors g n)))))
    (define c (if (valid? g int) int (set)))
    (if (> (set-count c) (set-count p)) c p)))

(printf "Part two: ~a\n" (string-join (sort (set->list (biggest g)) string<?) ","))
