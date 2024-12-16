#lang racket

(require racket/set)
(require "utils.rkt")

(day 15)
(testing #f)

;; Part One
(define (parse-input lines)
  (define-values (grid instrs) (splitf-at lines (λ (l) (not (equal? l "")))))
  (values
   (list->vector (map (compose list->vector string->list) grid))
   (flatten (map string->list (cdr instrs)))))

(define-values (grid instrs) (parse-input lines))
(define rows (vector-length grid))
(define cols (vector-length (vector-ref grid 0)))

(define (find-robot-start grid rows cols [i 0] [j 0])
  (cond
    ((equal? i rows) #f)
    ((equal? j cols) (find-robot-start grid rows cols (add1 i) 0))
    ((equal? #\@ (2d-ref grid i j)) (point i j))
    (else (find-robot-start grid rows cols i (add1 j)))))

(define (get-d instr)
  (match instr
    [#\< (point  0 -1)] [#\> (point 0 1)]
    [#\^ (point -1  0)] [#\v (point 1 0)]))

(define (get-chain grid i j d [acc (set)])
  (define-values (i^ j^) (values (+ i (point-x d)) (+ j (point-y d))))
  (match (2d-ref grid i j)
    [#\# #f]
    [#\. acc]
    [#\] #:when (or (equal? d (point -1 0)) (equal? d (point 1 0)))
     (define r1 (get-chain grid i^ j^ d (set-add acc (cons #\] (point i j)))))
     (define r2 (get-chain grid i^ (sub1 j^) d (set-add acc (cons #\[ (point i (sub1 j))))))
     (and r1 r2 (set-union r1 r2))]
    [#\[ #:when (or (equal? d (point -1 0)) (equal? d (point 1 0)))
     (define r1 (get-chain grid i^ j^ d (set-add acc (cons #\[ (point i j)))))
     (define r2 (get-chain grid i^ (add1 j^) d (set-add acc (cons #\] (point i (add1 j))))))
     (and r1 r2 (set-union r1 r2))]
    [c (get-chain grid i^ j^ d (set-add acc (cons c (point i j))))]))

(define (update-grid grid pos d)
  (define-values (i  j)  (values (point-x pos) (point-y pos)))
  (define-values (i^ j^) (values (+ i (point-x d)) (+ j (point-y d))))
  (define next (2d-ref grid i^ j^))
  (match next
    [#\# pos]
    [#\.
     (2d-set! grid i j #\.)
     (2d-set! grid i^ j^ #\@)
     (point i^ j^)]
    [(or #\O #\[ #\])
     (define chain (get-chain grid i j d))
     (cond
       (chain
        (for ([c chain]) ;; clear chain
          (2d-set! grid (point-x (cdr c)) (point-y (cdr c)) #\.))
        (for ([c chain]) ;; set new chain
          (2d-set! grid
                   (+ (point-x (cdr c)) (point-x d)) (+ (point-y (cdr c)) (point-y d))
                   (car c)))
        (point i^ j^))
       (else pos))]))

(define (go grid pos instrs)
  (match instrs
    ['() grid]
    [(cons instr instrs)
     (define grid^ (2d-vec-copy grid))
     (define pos^  (update-grid grid^ pos (get-d instr)))
     (go grid^ pos^ instrs)]))

(define (get-boxes grid rows cols b [i 0] [j 0])
  (cond
    ((equal? i rows) '())
    ((equal? j cols) (get-boxes grid rows cols b (add1 i) 0))
    ((equal? b (2d-ref grid i j)) (cons (point i j) (get-boxes grid rows cols b i (add1 j))))
    (else (get-boxes grid rows cols b i (add1 j)))))

(define final (go grid (find-robot-start grid rows cols) instrs))

(printf "Part one: ~a\n" (for/sum ([b (get-boxes final rows cols #\O)])
                           (+ (point-y b) (* 100 (point-x b)))))

;; Part Two
(define rows^ rows)
(define cols^ (* 2 cols))

(define (fatten-grid grid)
  (define grid^ (list->vector (for/list ([i (range rows^)]) (make-vector cols^ #\.))))
  (for* ([i (range rows)]
         [j (range cols)])
    (match (2d-ref grid i j)
      [#\#
       (2d-set! grid^ i (* 2 j) #\#)
       (2d-set! grid^ i (add1 (* 2 j)) #\#)]
      [#\O
       (2d-set! grid^ i (* 2 j) #\[)
       (2d-set! grid^ i (add1 (* 2 j)) #\])]
      [#\@
       (2d-set! grid^ i (* 2 j) #\@)]
      [else (void)]))
  grid^)

(define grid^ (fatten-grid grid))
(define final^ (go grid^ (find-robot-start grid^ rows^ cols^) instrs))

(printf "Part two: ~a\n" (for/sum ([b (get-boxes final^ rows^ cols^ #\[)])
                           (+ (point-y b) (* 100 (point-x b)))))
