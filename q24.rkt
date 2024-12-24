#lang racket

(require racket/set)
(require threading)
(require graph)
(require "utils.rkt")


(day 24)
(testing #f)

(define (assert c) (when (not c) (error "assertion error")))

;; Part One
(define (parse-input lines)
  (define-values (start gates) (splitf-at lines (negate (curry equal? ""))))
  (values
   (for/fold ([d (make-immutable-hash)])
             ([line start])
     (match-let ([(list _ var val) (regexp-match #px"(\\S*\\d*): (\\d)" line)])
       (dict-set d var (string->number val))))
   (for/fold ([d (make-immutable-hash)])
             ([line (cdr gates)])
     (match-let ([(list _ v1 rator v2 res)
                  (regexp-match #px"(\\S*\\d*) (AND|XOR|OR) (\\S*\\d*) -> (\\S*\\d*)" line)])
       (dict-set d res (list v1 v2 (cond ((equal? rator "AND") bitwise-and)
                                         ((equal? rator "XOR") bitwise-xor)
                                         ((equal? rator "OR" ) bitwise-ior))))))))

(define (build-graph gates)
  (directed-graph (for/fold ([edges '()])
                            ([(res gate) gates])
                    (match-let ([(list v1 v2  _) gate])
                      (append `((,v1 ,res) (,v2 ,res)) edges)))))

(define-values (start gates) (parse-input lines))

(define g (build-graph gates))

(define (run-gates order gates vals expected)
  (match order
    ['() vals]
    [(cons c order)
     (match-let ([(list v1 v2 rator) (dict-ref gates c)])
       (define v1^ (dict-ref vals v1 #f))
       (define v2^ (dict-ref vals v2 #f))
       (cond
         ((and v1^ v2^)
          (define out (rator v1^ v2^))
          (if (and expected (not (equal? out (dict-ref expected c out))))
              #f (run-gates order gates (dict-set vals c out) expected)))
         (else #f)))]))

(define (run start g gates [expected #f])
  (define out (run-gates (remove* (dict-keys start) (tsort g)) gates start expected))
  (if out
      (~> out
          in-dict-pairs sequence->list
          (filter (λ (x) (equal? #\z (first (string->list (car x))))) _)
          (sort _ (λ (a b) (string>? (car a) (car b))))
          (map (compose number->string cdr) _)
          (string-join _  "") (string->number _ 2))
      #f))

(printf "Part one: ~a\n" (run start g gates))

;; Part Two
(define (starts-with c) (λ (x) (equal? c (first (string->list x)))))

(define xs (map (curry dict-ref start) (sort (filter (starts-with #\x) (dict-keys start)) string>?)))
(define ys (map (curry dict-ref start) (sort (filter (starts-with #\y) (dict-keys start)) string>?)))
(define zs (sort (map car (~> gates in-dict-pairs sequence->list
                              (filter (λ (x) (equal? #\z (first (string->list (car x))))) _)))
                 string<?))
(define zs-expected (for/fold ([d (make-immutable-hash)])
                              ([z zs]
                               [ze (~> (+ (string->number (string-join (map number->string xs) "") 2)
                                          (string->number (string-join (map number->string ys) "") 2))
                                       (number->string _ 2) string->list
                                       (map (λ (x) (- (char->integer x) (char->integer #\0))) _)
                                       reverse)])
                      (dict-set d z ze)))

(define (x/y? c)
  (or (equal? #\x (first (string->list c)))
      (equal? #\y (first (string->list c)))))

(define (better-order q g^ [acc '()])
  (match q
    ['() acc]
    [(cons c q) #:when (not (x/y? c))
                (better-order (append q (get-neighbors g^ c)) g^ (cons c acc))]
    [(cons c q) (better-order q g^ acc)]))

(define (z-errors g gates)
  (for/fold ([errors '()])
            ([z zs])
    (define res (run-gates (better-order (list z) (transpose g)) gates start #f))
    (if (not (equal? (dict-ref res z) (dict-ref zs-expected z))) (cons z errors) errors)))

(define (update-gates! gates c)
  (match c
    ['() (void)]
    [(cons o1 (cons o2 c))
     (match-let ([(list in11 in12 _) (dict-ref gates o1)]
                 [(list in21 in22 _) (dict-ref gates o2)])
       (define tmp (dict-ref gates o1))
       (dict-set! gates o1 (dict-ref gates o2))
       (dict-set! gates o2 tmp)
       (update-gates! gates c))]))

;; two answers?? inconceivable!!
(define ans^  (list "z11" "wpd" "skh" "jqf" "z19" "cmp" "rhh" "smt"))
(define ans^^ (list "z11" "wpd" "skh" "jqf" "z19" "mdd" "z37" "wts"))

(define gates^ (make-hash (hash->list gates)))
(update-gates! gates^ ans^)
(define g^ (build-graph gates^))

(define gates^^ (make-hash (hash->list gates)))
(update-gates! gates^^ ans^^)
(define g^^ (build-graph gates^^))

(define expected-z (+ (string->number (string-join (map number->string xs) "") 2)
                      (string->number (string-join (map number->string ys) "") 2)))

(assert (equal? (run start g^ gates^) expected-z))
(assert (equal? (run start g^^ gates^^) expected-z))

(printf "Part two: ~a\n" (string-join (sort ans^^ string<?) ","))
