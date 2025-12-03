#lang racket

(define (invalid) (error 'calc "Invalid Expression"))

(define (skip-ws cs)
  (cond
    [(null? cs) '()]
    [(char-whitespace? (car cs)) (skip-ws (cdr cs))]
    [else cs]))

(define (only-ws? cs)
  (cond
    [(null? cs) #t]
    [(char-whitespace? (car cs)) (only-ws? (cdr cs))]
    [else #f]))


(define (read-int cs)
  (define start (skip-ws cs))
  (let loop ([cs start] [digits '()])
    (cond
      [(and (pair? cs) (char-numeric? (car cs)))
       (loop (cdr cs) (cons (car cs) digits))]
      [else
       (if (null? digits)
           (invalid)
           (let* ([s (list->string (reverse digits))]
                  [n (string->number s)])
             (if (and n (integer? n))
                 (cons n cs)
                 (invalid))))])))

(define (history-length h) (length h))

(define (history-get h id)
  (define len (length h))
  (when (or (< id 1) (> id len)) (invalid))
  (let find ([h h] [k (- len id)])
    (cond
      [(null? h) (invalid)]
      [(zero? k) (car h)]
      [else (find (cdr h) (sub1 k))])))

(define (parse-expr cs history)
  (define start (skip-ws cs))
  (when (null? start) (invalid))
  (define c (car start))
  (define rest (cdr start))
  (cond
    [(char=? c #\+)  
     (define p1 (parse-expr rest history))
     (define v1 (car p1))
     (define p2 (parse-expr (cdr p1) history))
     (define v2 (car p2))
     (cons (+ v1 v2) (cdr p2))]

    [(char=? c #\*) 
     (define p1 (parse-expr rest history))
     (define v1 (car p1))
     (define p2 (parse-expr (cdr p1) history))
     (define v2 (car p2))
     (cons (* v1 v2) (cdr p2))]

    [(char=? c #\/) 
     (define p1 (parse-expr rest history))
     (define v1 (car p1))
     (define p2 (parse-expr (cdr p1) history))
     (define v2 (car p2))
     (when (= v2 0) (invalid))
     (cons (quotient v1 v2) (cdr p2))]

    [(char=? c #\-) 
     (define p (parse-expr rest history))
     (define v (car p))
     (cons (- v) (cdr p))]

    [(char=? c #\$) 
      (define r (read-int rest))
      (define id (car r))
      (define val (history-get history id))
      (cons val (cdr r))]

    [(char-numeric? c)
     (read-int start)]

    [else (invalid)]))