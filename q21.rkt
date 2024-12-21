#lang racket

(require racket/set)
(require threading)
(require "utils.rkt")

(day 21)
(testing #f)

;; Part One
(define (n->p n)
  (match n
    [#\7 (point 0 0)] [#\8 (point 0 1)] [#\9 (point 0 2)]
    [#\4 (point 1 0)] [#\5 (point 1 1)] [#\6 (point 1 2)]
    [#\1 (point 2 0)] [#\2 (point 2 1)] [#\3 (point 2 2)]
                      [#\0 (point 3 1)] [#\A (point 3 2)]))

(define (dist p1 p2)
  (+ (abs (- (point-x p1) (point-x p2))) (abs (- (point-y p1) (point-y p2)))))

(define (get-directions cur dest)
  (define di (- (point-x dest) (point-x cur)))
  (define dj (- (point-y dest) (point-y cur)))
  (cond
    ((and (negative? di) (negative? dj))
     (append (ls-of (abs di) (point 0 1)) (ls-of (abs dj) (point 1 0))))
    ((negative? di)
     (append (ls-of (abs di) (point 0 1)) (ls-of (abs dj) (point 1 2))))
    ((negative? dj)
     (append (ls-of (abs di) (point 1 1)) (ls-of (abs dj) (point 1 0))))
    (else
     (append (ls-of (abs di) (point 1 1)) (ls-of (abs dj) (point 1 2))))))

(define (valid? hole c dirs)
  (if (equal? c hole)
      #f
      (match dirs
        ['() #t]
        [(cons (point 1 1) dirs) (valid? hole (point (add1 (point-x c)) (point-y c)) dirs)]
        [(cons (point 0 1) dirs) (valid? hole (point (sub1 (point-x c)) (point-y c)) dirs)]
        [(cons (point 1 0) dirs) (valid? hole (point (point-x c) (sub1 (point-y c))) dirs)]
        [(cons (point 1 2) dirs) (valid? hole (point (point-x c) (add1 (point-y c))) dirs)])))

(define (num-pad-path nums [cur (point 3 2)])
  (match nums
    ['() '(())]
    [(cons n nums)
     (define directions (get-directions cur (n->p n)))
     (define rets (num-pad-path nums (n->p n)))
     (for*/set ([r rets] [p (filter (curry valid? (point 3 0) cur) (permutations directions))])
       (append p (list (point 0 2)) r))]))

(define (priority p)
  (match p
    [(point 1 2) 0]   ;; > (this was 4 but I changed it to 0 and now it works???) 
    [(point 1 0) 3]   ;; <
    [(point 1 1) 2]   ;; v
    [(point 0 1) 1])) ;; ^

(define (sort-fn p1 p2)
  (> (priority p1) (priority p2)))

(define (priority-no-hole p)
  (match p
    [(point 1 2) 4]   ;; > 
    [(point 0 1) 3]   ;; ^
    [(point 1 1) 2]   ;; v
    [(point 1 0) 1])) ;; <

(define (sort-fn-no-hole p1 p2)
  (> (priority-no-hole p1) (priority-no-hole p2)))

(define memo (make-hash))
(define (raise-dirs ds [p (point 0 2)])
  
  (cond
    ((dict-has-key? memo (cons ds p)) (dict-ref memo (cons ds p)))
    (else
     (match ds
       ['() '()]
       [(cons d ds)
        (define ret (append (raise d p) (raise-dirs ds d)))
        (dict-set! memo (cons (cons d ds) p) ret)
        ret]))))

(define (raise c p)
  (cond
    ((equal? c p) (list (point 0 2)))
    (else
     (define dirs (sort (get-directions p c) sort-fn))
     (append (if (valid? (point 0 0) p dirs) dirs
                 (sort (get-directions p c) sort-fn-no-hole))
                  (list (point 0 2))))))

(define memo^ (make-hash))
(define (raise-n x n)
  (cond
    ((dict-has-key? memo^ (cons x n)) (dict-ref memo^ (cons x n)))
    ((zero? n) (length x))
    (else
     (define paths (for/list ([d x] [p (cons (point 0 2) (take x (sub1 (length x))))])
                     (raise-dirs (list d) p)))
     (define ret (for/sum ([p paths])
                   (raise-n p (sub1 n))))
     (dict-set! memo^ (cons x n) ret)
     ret)))

(define (get-length code n-raises)
  (inexact->exact
   (for/fold ([m +inf.0]) ([x (~> code string->list num-pad-path)])
     (min m (raise-n x n-raises)))))


(printf "Part one: ~a\n" (for/sum ([line lines])
                           (* (string->number (substring line 0 (sub1 (string-length line))))
                              (get-length line 2))))

;; Part Two
(printf "Part two: ~a\n" (for/sum ([line lines])
                           (* (string->number (substring line 0 (sub1 (string-length line))))
                              (get-length line 25))))

