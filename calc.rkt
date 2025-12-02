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