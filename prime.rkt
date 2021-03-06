#!/usr/bin/racket -f

#lang racket

(define  (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ 1 m) n))))
(define (sieve l)
  (define (remove-multiples n l)
    (if (null? l)
         '()
         (if  (= (modulo (car l) n) 0)      ; division test
              (remove-multiples n (cdr l))
              (cons (car l)
                    (remove-multiples n (cdr l))))))
  (if (null? l)
      '()
      (cons (car l)
             (sieve (remove-multiples (car l) (cdr l))))))
(define (primes<= n)
  (sieve (interval-list 2 n)))

(define arg (string->number (vector-ref (current-command-line-arguments) 0)))
(display (primes<= arg))
(newline)
