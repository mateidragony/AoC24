#lang racket

(require racket/set)
(require "utils.rkt")

(day 12)
(testing #f)

;; Part One
(struct perimiter (d r c) #:transparent)

(define (get-neighbors i j)
  (list (point (add1 i) j) (point (sub1 i) j)
        (point i (add1 j)) (point i (sub1 j))))

(define (get-group-neighbors i j chars type)
  (filter (λ (p) (equal? type (2d-ref-default chars (point-x p) (point-y p) #f)))
          (get-neighbors i j)))

(define (mk-perimiter i j i^ j^)
  (perimiter (cond ((equal? i^ (add1 i)) 'd) ((equal? i^ (sub1 i)) 'u)
                   ((equal? j^ (add1 j)) 'r) ((equal? j^ (sub1 j)) 'l))
             i j))

(define (get-perimiters i j chars type)
  (list->set
   (map (λ (p) (mk-perimiter i j (point-x p) (point-y p)))
        (filter (λ (p) (not (equal? type (2d-ref-default chars (point-x p) (point-y p) #f))))
                (get-neighbors i j)))))

(define (get-valid-group-neighbors i j type chars visited)
  (filter (λ (p) (and (in-bounds (point-x p) (point-y p) chars)
                      (not (set-member? visited p))))
          (get-group-neighbors i j chars type)))

(define (count-sides-greedy ps [i^ #f] [j^ #f])
  (match ps
    ['() 0]
    [(cons (point i j) ps)
     (cond
       ((not i^) (add1 (count-sides-greedy ps i j)))
       ((not (equal? i i^))        (add1 (count-sides-greedy ps i j)))
       ((not (equal? j (add1 j^))) (add1 (count-sides-greedy ps i j)))
       (else (count-sides-greedy ps i j)))]))

(define (count-sides-dir ps d)
  (match d
    [(or 'u 'd) (count-sides-greedy
                 (map (λ (p) (point (perimiter-r p) (perimiter-c p)))
                      (sort (sort ps (λ (p1 p2) (< (perimiter-c p1) (perimiter-c p2))))
                            (λ (p1 p2) (< (perimiter-r p1) (perimiter-r p2))))))]
    [(or 'l 'r) (count-sides-greedy
                 (map (λ (p) (point (perimiter-c p) (perimiter-r p)))
                      (sort (sort ps (λ (p1 p2) (< (perimiter-r p1) (perimiter-r p2))))
                       (λ (p1 p2) (< (perimiter-c p1) (perimiter-c p2))))))]))

(define (count-sides perimiters)
  (define p-gs (group-by perimiter-d (set->list perimiters)))
  (for/sum ([p p-gs]) (count-sides-dir p (perimiter-d (first p)))))

(define (get-cost q type chars p1? [a 0] [p (set)] [visited (set)])
  (cond
    ((set-empty? q) (if p1? (values (* a (set-count p)) visited)
                            (values (* a (count-sides p)) visited)))
    (else
     (define pt (set-first q))
     (define vns (get-valid-group-neighbors (point-x pt) (point-y pt) type chars (set-add visited pt)))
     (get-cost (set-union (set-remove q pt) (list->set vns)) type chars p1? (add1 a)
               (set-union p (get-perimiters (point-x pt) (point-y pt) chars type))
               (set-add visited pt)))))

(define (get-total-cost chars p1? [i 0] [j 0] [visited (set)])
  (cond
    ((equal? i rows) 0)
    ((equal? j cols) (get-total-cost chars p1? (add1 i) 0 visited))
    ((set-member? visited (point i j)) (get-total-cost  chars p1? i (add1 j) visited))
    (else
     (define-values (cost visited^) (get-cost (set (point i j)) (2d-ref chars i j) chars p1?))
     (+ cost (get-total-cost chars p1? i (add1 j) (set-union visited visited^))))))


(printf "Part one: ~a\n" (get-total-cost chars #t))

;; Part Two
(printf "Part two: ~a\n" (get-total-cost chars #f))
