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