#lang racket

(require racket/set)
(require "utils.rkt")

(day 9)
(testing #f)

;; Part One                
(define (parse-input line free?)
  (match line
    ['() (values '() '())]
    [(cons f line)
     (define-values (files free) (parse-input line (not free?)))
     (if free?
         (values files (cons (- (char->integer f) (char->integer #\0)) free))
         (values (cons (- (char->integer f) (char->integer #\0)) files) free))]))

(define (filelens->ids files [id 0])
  (match files
    ['() '()]
    [(cons f files)
     (append (ls-of f id) (filelens->ids files (add1 id)))]))

(define (end-of-block files idx [found-block #f])
  (cond
    ((null? files) 0)
    ((and found-block (not (equal? idx (car files)))) 0)
    ((equal? idx (car files)) (add1 (end-of-block (cdr files) idx #t)))
    (else (add1 (end-of-block (cdr files) idx #f)))))

(define (decrease-end ls)
  (cond
    ((null? ls) ls)
    ((equal? 0 (last ls)) (decrease-end (take ls (sub1 (length ls)))))
    (else (list-set ls (sub1 (length ls)) (sub1 (last ls))))))

(define (pack-files files filesnums free acc)
  (cond
    ((null? files) acc)
    ((null? free) acc)
    ((equal? 0 (car free)) (pack-files (drop files (car filesnums))
                                       (cdr filesnums) (cdr free)
                                       (append acc (ls-of (car filesnums) (car files)))))
    (else (pack-files (take files (- (length files) 1)) (decrease-end filesnums)
                      (list-set free 0 (sub1 (car free)))
                      (append acc (last-pair files))))))

(define (check-sum ls [idx 0])
  (match ls
    ['() 0]
    [(cons x ls) (+ (* x idx) (check-sum ls (add1 idx)))]))

(define-values (files free) (parse-input (string->list (car lines)) #f))
(define fileids (filelens->ids files))
(define packed  (pack-files (drop fileids (end-of-block fileids 0)) (cdr files) free
                            (take fileids (end-of-block fileids 0))))

(printf "Part one: ~a\n" (check-sum packed))

;; Part Two
(struct file-str (sz id) #:transparent) ;; Should've just done this for Part One but I was stubborn

(define files^ (for/list ([f files] [i (range (length files))])
                 (file-str f i)))

(struct file-idx (f idx) #:transparent)

(define (files->fileidxs files free [prev 0])
  (match* (files free)
    [('() '()) (values '() '())]
    [((cons f files) (cons fr free))
     (define-values (fileidxs freeidxs) (files->fileidxs files free (+ prev fr (file-str-sz f))))
     (values (cons (file-idx f (+ prev fr)) fileidxs)
             (cons (file-idx fr prev) freeidxs))]))

(define-values (file-idxs free-idxs) (files->fileidxs files^ (cons 0 free)))

(define (find-fit^ free f [idx 0])
  (match free
    ['() #f]
    [(cons sz free) #:when (<= (file-str-sz f) sz) idx]
    [(cons _ free) (find-fit^ free f (add1 idx))]))

(define len-first (file-str-sz (first files^)))

(define (pack-files^^ file-idxs free-idxs acc)
  (match file-idxs
    ['() acc]
    [(cons (file-idx f idx) files)
     (define fit (find-fit^ (map file-idx-f free-idxs) f))
     (define fr (if fit (list-ref free-idxs fit) #f))
     (if (and fit (< (file-idx-idx fr) idx))
         (pack-files^^ files
                       (list-set free-idxs fit (file-idx (- (file-idx-f fr) (file-str-sz f))
                                                         (+ (file-idx-idx fr) (file-str-sz f))))
                       (cons (file-idx f (file-idx-idx fr)) acc))
         (pack-files^^ files free-idxs (cons (file-idx f idx) acc)))]))

(define (check-sum^^ ls)
  (match ls
    ['() 0]
    [(cons (file-idx (file-str sz id) idx) ls)
     (+ (sum (map (λ (x) (* id x)) (range idx (+ idx sz)))) (check-sum^^ ls))]))

(define packed^ (pack-files^^ (reverse file-idxs) (cdr free-idxs) '()))
(printf "Part two: ~a\n" (check-sum^^ packed^))
